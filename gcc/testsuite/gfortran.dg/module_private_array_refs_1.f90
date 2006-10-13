! { dg-do compile }
! This tests the fix for PR28735 in which an ICE would be triggered in resolve_ref
! because the references to 'a' and 'b' in the dummy arguments of mysub have
! no symtrees in module bar, being private there.
!
! Contributed by  Andrew Sampson  <adsspamtrap01@yahoo.com>
!
!-- foo.F -----------------------------------------------
module foo
  implicit none
  public
  integer, allocatable :: a(:), b(:)
end module foo

!-- bar.F ---------------------------------------------
module bar
  use foo
  implicit none
  private                !  This triggered the ICE
  public :: mysub        !  since a and b are not public

contains

  subroutine mysub(n, parray1)
    integer, intent(in) :: n
    real, dimension(a(n):b(n)) :: parray1
    if ((n == 1) .and. size(parray1, 1) /= 10) call abort ()
    if ((n == 2) .and. size(parray1, 1) /= 42) call abort ()
  end subroutine mysub
end module bar

!-- sub.F -------------------------------------------------------
subroutine sub()

  use foo
  use bar
  real :: z(100)
  allocate (a(2), b(2))
  a = (/1, 6/)
  b = (/10, 47/)
  call mysub (1, z)
  call mysub (2, z)

  return
end

!-- MAIN ------------------------------------------------------
  use bar
  call sub ()
end

! { dg-final { cleanup-modules "foo bar" } }
