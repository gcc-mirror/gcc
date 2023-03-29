! { dg-do compile }
!
! PR fortran/108450
! This program used to cause an ICE because of the double resolution
! of the maxloc expression and the addition of a hidden unnamed argument
! during the first resolution.
!
! Original testcase from G. Steinmetz

subroutine s1
   integer :: a(8) = 0
   integer :: l
   integer :: n
   !$omp atomic
   n = maxloc(a, mask=l) ! { dg-error ".mask. argument of .maxloc. intrinsic at .1. must be LOGICAL" }
end

subroutine s2
   integer :: a(8) = 0
   integer :: l
   integer :: n
   !$omp atomic
   n = minloc(a, mask=l) ! { dg-error ".mask. argument of .minloc. intrinsic at .1. must be LOGICAL" }
end

subroutine s3
   integer :: a(8) = 0
   integer :: l
   integer :: n
   !$omp atomic
   n = findloc(a, 3, mask=l) ! { dg-error ".mask. argument of .findloc. intrinsic at .1. must be LOGICAL" }
end
