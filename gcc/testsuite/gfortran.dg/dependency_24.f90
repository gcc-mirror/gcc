! { dg-do run }
! Check the fix for PR38863 comment #1, where defined assignment
! to derived types was not treating components correctly that were
! not set explicitly.
!
! Contributed by Mikael Morin  <mikael@gcc.gnu.org>
!
module m
  type t
    integer :: i,j
  end type t
  type ti
    integer :: i,j = 99
  end type ti
  interface assignment (=)
    module procedure i_to_t, i_to_ti
  end interface
contains 
  elemental subroutine i_to_ti (p, q)
    type(ti), intent(out) :: p
    integer, intent(in)  :: q
    p%i = q
  end subroutine
  elemental subroutine i_to_t (p, q)
    type(t), intent(out) :: p
    integer, intent(in)  :: q
    p%i = q
  end subroutine
end module

  use m
  call test_t  ! Check original problem
  call test_ti ! Default initializers were treated wrongly
contains
  subroutine test_t
    type(t), target :: a(3)
    type(t), target  :: b(3)
    type(t), dimension(:), pointer :: p
    logical :: l(3)

    a%i = 1
    a%j = [101, 102, 103]
    b%i = 3
    b%j = 4

    p => b
    l = .true.

    where (l)
      a = p%i         ! Comment #1 of PR38863 concerned WHERE assignment
    end where
    if (any (a%j .ne. [101, 102, 103])) call abort

    a = p%i           ! Ordinary assignment was wrong too.
    if (any (a%j .ne. [101, 102, 103])) call abort
  end subroutine

  subroutine test_ti
    type(ti), target :: a(3)
    type(ti), target  :: b(3)
    type(ti), dimension(:), pointer :: p
    logical :: l(3)

    a%i = 1
    a%j = [101, 102, 103]
    b%i = 3
    b%j = 4

    p => b
    l = .true.

    where (l)
      a = p%i
    end where
    if (any (a%j .ne. 99)) call abort

    a = p%i
    if (any (a%j .ne. 99)) call abort
  end subroutine
end
! { dg-final { cleanup-modules "m" } }
