! { dg-do compile }
! { dg-options "-fcray-pointer" }

! Bad type for pointer
subroutine err1
  real ipt
  real array(10)
  pointer (ipt, array) ! { dg-error "integer" }
end subroutine err1

! Multiple declarations for the same pointee
subroutine err2
  real array(10)
  pointer (ipt1, array)
  pointer (ipt2, array) ! { dg-error "multiple" }
end subroutine err2

! Vector assignment to an assumed size array
subroutine err3
  real target(10)
  real array(*)
  pointer (ipt, array)
  ipt = loc (target)
  array = 0    ! { dg-error "Vector assignment" }
end subroutine err3

subroutine err4
  pointer (ipt, ipt) ! { dg-error "POINTER attribute" }
end subroutine err4

! duplicate array specs
subroutine err5
  pointer (ipt, array(7))
  real array(10)      ! { dg-error "Duplicate array" }  
end subroutine err5

subroutine err6
  real array(10)
  pointer (ipt, array(7))  ! { dg-error "Duplicate array" }
end subroutine err6

! parsing stuff
subroutine err7
  pointer (                  ! { dg-error "variable name" }
  pointer (ipt               ! { dg-error "Expected" }
  pointer (ipt,              ! { dg-error "variable name" }
  pointer (ipt,a1            ! { dg-error "Expected" }
  pointer (ipt,a2),          ! { dg-error "Expected" }
  pointer (ipt,a3),(         ! { dg-error "variable name" }
  pointer (ipt,a4),(ipt2     ! { dg-error "Expected" }
  pointer (ipt,a5),(ipt2,    ! { dg-error "variable name" }
  pointer (ipt,a6),(ipt2,a7  ! { dg-error "Expected" }
end subroutine err7

! more attributes
subroutine err8(array)
  real array(10)
  integer dim(2)
  integer, pointer :: f90ptr
  integer, target :: f90targ
  pointer (ipt, array)    ! { dg-error "DUMMY" }
  pointer (dim, elt1)     ! { dg-error "DIMENSION" }
  pointer (f90ptr, elt2)  ! { dg-error "POINTER" }
  pointer (ipt, f90ptr)   ! { dg-error "POINTER" }
  pointer (f90targ, elt3) ! { dg-error "TARGET" }
  pointer (ipt, f90targ)  ! { dg-error "TARGET" }
end subroutine err8

