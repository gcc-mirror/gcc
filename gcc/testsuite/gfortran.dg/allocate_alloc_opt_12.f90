! { dg-do compile }
!
! PR 45507: [4.6 Regression] Bogus Error: Can't convert TYPE(c_ptr) to INTEGER(4)
!
! Contributed by Andrew Benson <abenson@its.caltech.edu>

 use, intrinsic :: iso_c_binding

 type :: cType
   type(c_ptr) :: accelPtr = c_null_ptr
 end type cType

 type(cType), allocatable, dimension(:) :: filters
 class(cType), allocatable :: f

 allocate(filters(1))
 allocate(f,MOLD=filters(1))

end
