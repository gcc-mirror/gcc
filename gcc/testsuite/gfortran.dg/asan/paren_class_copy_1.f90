! { dg-do run }
!
! PR 128478
! A parenthesized scalar CLASS variable passed as an actual argument is a
! copy; without the fix the dummy aliased the variable, and a TYPE dummy
! segfaulted.  Checked under -fsanitize=address.

module m
  implicit none
  type :: t
    integer, allocatable :: v(:)
  end type
contains
  subroutine sq (x, z)
    class(*), intent(in) :: x
    complex, intent(inout) :: z
    z = (0., 0.)
    select type (x)
    type is (complex)
      if (x /= (5., 6.)) stop 1
    class default
      stop 2
    end select
  end subroutine
  subroutine st (x, z)
    class(t), intent(in) :: x
    type(t), intent(inout) :: z
    z%v = [0, 0]
    if (any (x%v /= [5, 6])) stop 3
  end subroutine
  subroutine s1 (x)
    type(t), intent(in) :: x
    if (any (x%v /= [5, 6])) stop 6
  end subroutine
  subroutine sc (x, c)
    class(*), intent(in) :: x
    character(*), intent(inout) :: c
    c = 'xyz'
    select type (x)
    type is (character(*))
      if (len (x) /= 3 .or. x /= 'abc') stop 8
    class default
      stop 9
    end select
  end subroutine
end module

program p
  use m
  implicit none
  complex, target :: zs = (5., 6.)
  character(3), target :: cc = 'abc'
  type(t), target :: ts
  class(*), pointer :: q
  class(t), pointer :: qt
  class(t), allocatable :: cs

  q => zs
  call sq ((q), zs)
  if (zs /= (0., 0.)) stop 4

  q => cc
  call sc ((q), cc)
  if (cc /= 'xyz') stop 10

  ts = t ([5, 6])
  qt => ts
  call st ((qt), ts)
  if (any (ts%v /= 0)) stop 5

  allocate (cs, source = t ([5, 6]))
  call s1 ((cs))
  if (any (cs%v /= [5, 6])) stop 7
end program
