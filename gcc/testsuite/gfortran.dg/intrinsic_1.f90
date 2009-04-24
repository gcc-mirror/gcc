! { dg-do compile }
!
! PR 39861/39864
!
! Test cases provided by Dominique d'Humieres <dominiq@lps.ens.fr>
! and Michael Richmond <michael.a.richmond@nasa.gov>.

module vector_calculus
  intrinsic :: dot_product, sqrt

contains

  function len(r)
    real, dimension(:), intent(in) :: r
    real :: len
    len = sqrt(dot_product(r,r))
  end function len

  FUNCTION next_state()
    INTRINSIC :: RESHAPE
    INTEGER, PARAMETER :: trantb(1,1) = RESHAPE((/1,2/), shape=(/1,1/))
    next_state = trantb(1, 1)
  END FUNCTION next_state

end module vector_calculus

! { dg-final { cleanup-modules "vector_calculus" } }

