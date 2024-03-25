! { dg-additional-options "-fcoarray=lib" }
!
! PR fortran/93660
!
! Failed as TREE_TYPE(fndecl) did not include the
! hidden caf_token/caf_offset arguments.
!
integer function f(x)
   integer :: x[*]
   !$omp declare simd
   f = x[1]
end
