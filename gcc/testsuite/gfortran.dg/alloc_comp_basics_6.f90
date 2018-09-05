! { dg-do compile }
!
! PR 58026: Bad error recovery for allocatable component of undeclared type
!
! Contributed by Joost VandeVondele <Joost.VandeVondele@mat.ethz.ch>

  type sysmtx_t
     type(ext_complex_t), allocatable :: S(:)  ! { dg-error "has not been declared" }
     class(some_type), allocatable :: X        ! { dg-error "has not been declared" }
  end type

end
