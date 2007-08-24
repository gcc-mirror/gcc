! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/33178
!
! Initialization expressions:
! Fortran 95: Elemental functions w/ integer/character arguments
! Fortran 2003: restriction lifted
!
integer :: a = sign(1,1)   ! Ok F95
real    :: b = sign(1.,1.) ! { dg-error "Fortran 2003: Elemental function as initialization expression" }
end
