! { dg-additional-options "-fcoarray=lib" }
!
! PR fortran/93660
!
! Failed as TREE_TYPE(fndecl) did not include the
! hidden caf_token/caf_offset arguments.
!
integer function f(x)	! { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } }
   integer :: x[*]
   !$omp declare simd
   f = x[1]
end
