!
! { dg-do compile }
!
! PR fortran/119836
!
! Although intrinsic functions contained within the Fortran standard
! are pure procedures, many of the additional intrinsic functions
! supplied in libgfortran are impure.  RAND() is one such function.
!
program foo
   implicit none
   integer i
   real x(4)
   do concurrent (i=1:4)
      x = rand()     ! { dg-error "Reference to impure function" }
      block
         x = rand()  ! { dg-error "Reference to impure function" }
      end block
   end do
   print *, x
end program foo
