! { dg-do run }
! { dg-options "-finit-real=NAN" }
! { dg-add-options ieee }
!
! PR fortran/50619
!
! Contributed by Fred Krogh
!
! The NaN initialization used to set the associate name to NaN!
!

module testa2
type, public ::  test_ty
  real :: rmult = 1.0e0
end type test_ty

contains
  subroutine test(e, var1)
    type(test_ty) :: e
    real :: var1, var2 ! Should get NaN initialized

    ! Should be the default value
    if (e%rmult /= 1.0) STOP 1

    ! Check that NaN initialization is really turned on
    if (var1 == var1) STOP 2 
    if (var2 == var2) STOP 3 

    ! The following was failing:
    associate (rmult=>e%rmult)
      if (e%rmult /= 1.0) STOP 4
    end associate
  end subroutine test
end module testa2

program testa1
  use testa2
  type(test_ty) :: e
  real :: var1 ! Should get NaN initialized
  call test(e, var1)
  stop
end program testa1
