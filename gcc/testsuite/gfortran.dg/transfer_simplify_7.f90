! { dg-do compile }
! PR fortran/34495 - accepts invalid init-expr with TRANSFER

! 'b' is implicitly typed
real :: a = transfer(1234, b)              ! { dg-error "does not reduce to a constant" }

! 'c' is used on lhs and rhs
real :: c = transfer(1234, c)              ! { dg-error "does not reduce to a constant" }

! 'bp' is implicitly typed
real, parameter :: ap = transfer(1234, bp) ! { dg-error "does not reduce to a constant" }

! 'yp' is used on lhs and rhs
real, parameter :: cp = transfer(1234, cp) ! { dg-error "before its definition is complete" }


! same with arrays
real, dimension(2) :: a2 = transfer([1, 2], b2)  ! { dg-error "does not reduce to a constant" }

real, dimension(2) :: a2 = transfer([1, 2], b2)  ! { dg-error "does not reduce to a constant" }

dimension :: bp(2)
real, parameter, dimension(2) :: ap2 = transfer([1, 2], bp2)  ! { dg-error "does not reduce to a constant" }

real, parameter, dimension(2) :: cp2 = transfer([1, 2], cp2)  ! { dg-error "before its definition is complete" }


! same with matrices
real, dimension(2,2) :: a3 = transfer([1, 2, 3, 4], b3)  ! { dg-error "does not reduce to a constant" }

real, dimension(2,2) :: a3 = transfer([1, 2, 3, 4], b3)  ! { dg-error "does not reduce to a constant" }

dimension :: bp3(2,2)
real, parameter, dimension(2,2) :: ap3 = transfer([1, 2, 3, 4], bp3)  ! { dg-error "does not reduce to a constant" }

real, parameter, dimension(2,2) :: cp3 = transfer([1, 2, 3, 4], cp3)  ! { dg-error "before its definition is complete" }

end
