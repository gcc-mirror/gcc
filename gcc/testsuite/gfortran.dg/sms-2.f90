! { dg-do run }
! { dg-options "-O2 -fmodulo-sched" }
! This testcase related to wrong order within a cycle fix.
!
program foo 
  real, dimension (5, 5, 5, 5) :: a

  a (:, :, :,  :)  = 4
  a (:, 2, :, 4) = 10
  a (:, 2, :, 1) = 0

  forall (i = 1:5, i == 3) 
     a(i, i, i, i) = -5
   end forall

  if (sum (a) .ne. 2541.0) call abort ()
end


