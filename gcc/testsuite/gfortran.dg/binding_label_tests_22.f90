! { dg-do compile }
!
! PR fortran/48858
!
subroutine foo() ! { dg-error "Global name 'foo' at .1. is already being used as a SUBROUTINE at .2." }
entry foo() ! { dg-error "Global name 'foo' at .1. is already being used as a SUBROUTINE at .2." }
end subroutine foo

