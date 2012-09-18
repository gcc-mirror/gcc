! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/54225
!

integer, allocatable :: a[:,:]

allocate (a[*,4]) ! { dg-error "Unexpected '.' for codimension 1 of 2" }
end
