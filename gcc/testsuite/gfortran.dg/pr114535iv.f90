! Compiled with pr114535d.f90
! Contributed by Andrew Benson  <abensonca@gcc.gnu.org>
!
module iv
  type, public :: vs
     integer :: i
   contains
     final :: destructor
  end type vs
  integer :: ctr = 0
contains
  impure elemental subroutine destructor(s)
    type(vs), intent(inout) :: s
    s%i = 0
    ctr = ctr + 1
  end subroutine destructor
end module iv

