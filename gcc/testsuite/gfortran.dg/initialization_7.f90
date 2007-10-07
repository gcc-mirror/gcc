! { dg-do compile }
!
! PR fortran/31253 -- ICE on invlid initialization expression
! Contributed by: Mikael Morin <mikael DOT morin AT tele2 DOT fr>
!

subroutine probleme(p)
  real(kind=8), dimension(:) :: p
  integer :: nx = size(p, 1)          ! { dg-error "Deferred array" }
  integer :: nix

  nix = nx
end subroutine
