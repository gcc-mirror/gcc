! { dg-do compile }

! Check that a missing call does not cause a segfault

module m
use iso_c_binding
implicit none(type,external)
contains
subroutine f(x,y,z)
  type(c_ptr) :: x,y,z
end
subroutine g(x,y,z)
  type(c_ptr) :: x,y,z
  !$omp declare variant(f) adjust_args(need_device_ptr: x,y) adjust_args(nothing : z,x) match(construct={dispatch})
end
end 

use m
implicit none(type,external)
  type(c_ptr) :: a,b,c
  !$omp dispatch
     g(a,b,c) ! { dg-error "'g' at .1. is not a variable" }
    ! Should be: call g(a,b,c)
end ! { dg-error "Unexpected END statement at .1." }
! { dg-error "Unexpected end of file in .*" "" { target *-*-* } 0 }
