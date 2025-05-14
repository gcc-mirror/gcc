! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008" }
! PR  Fortran/80768
! Reported by Vittorio Zecca.
program foo
   implicit none
   integer k5
   k5 = num_images(failed=.false.) ! { dg-error "Cannot find keyword named 'failed' in call to 'num_images'" }
end program foo
