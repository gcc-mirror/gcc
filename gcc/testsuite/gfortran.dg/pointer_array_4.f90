! { dg-do run }
!
! Test the fix for PR57116 as part of the overall fix for PR34640.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_rtti_ptr
  implicit none
  type :: foo
     real :: v
     integer :: i
  end type foo
contains
  subroutine extract(this, v, ic)
    class(*), target :: this(:)
    real, pointer :: v(:)
    integer :: ic
    select type (this)
    type is (real)
       v => this(ic:)
    class is (foo)
       v => this(ic:)%v
    end select
  end subroutine extract
end module

program prog_rtti_ptr
  use mod_rtti_ptr
  class(*), allocatable, target :: o(:)
  real, pointer :: v(:)

  allocate(o(3), source=[1.0, 2.0, 3.0])
  call extract(o, v, 2)
  if (size(v) == 2 .and. all (v == [2.0, 3.0])) then
     deallocate(o)
  else
     STOP 1
  end if

  allocate(o(3), source=[foo(1.0, 1), foo(4.0, 4), foo(5.0, 5)])
  call extract(o, v, 2)
  if (size(v) == 2 .and. all (v == [4.0, 5.0])) then
     deallocate(o)
  else
     STOP 2
  end if

! The rest tests the case in comment 2 <janus@gcc.gnu.org>

  call extract1 (v, 1)
  if (any (v /= [1.0, 2.0])) STOP 3
  call extract1 (v, 2)  ! Call to deallocate pointer.

contains
  subroutine extract1(v, flag)
    type :: foo
       real :: v
       character(4) :: str
    end type
    class(foo), pointer, save :: this(:)
    real, pointer :: v(:)
    integer :: flag

    if (flag == 1) then
      allocate (this(2), source = [foo (1.0, "one "), foo (2.0, "two ")])
      select type (this)
        class is (foo)
          v => this(1:2)%v
      end select
    else
      deallocate (this)
    end if
  end subroutine

end program prog_rtti_ptr
