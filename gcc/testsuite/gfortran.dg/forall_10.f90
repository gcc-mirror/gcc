! { dg-do run }
! { dg-options "-O" }
! Tests the fix for PR30400, in which the use of ANY in the
! FORALL mask was rejected.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
program pr30400_1
  real, dimension (5, 5, 5, 5) :: a

  a (:, :, :,  :)  = 4
  a (:, 2, :, 4) = 10
  a (:, 2, :, 1) = 0

  forall (i = 1:5, j = 1:5, k = 1:5, any (a (i, j, k,  :)  .gt. 6))
    forall (l = 1:5, any (a (:, :, :, l) .lt. 2))
      a (i, j, k, l) = i - j + k - l
    end forall
  end forall
  if (sum (a) .ne. 2625.0) call abort ()

 ! Check that the fix has not broken the treatment of the '=='
  forall (i = 1:5, i == 3) a(i, i, i, i) = -5
  if (sum (a) .ne. 2616.0) call abort ()
end
