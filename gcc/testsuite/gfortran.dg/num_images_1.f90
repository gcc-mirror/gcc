! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008" }
! PR  Fortran/80768
!
program foo
   implicit none
   integer k5
   k5 = num_images(failed=.false.) ! { dg-error "argument to NUM_IMAGES" }
   print *, k5
end program foo
