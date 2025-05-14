! { dg-do run }
!
! PR119460: Scalar reduce was failing with ARRAY elements larger than
! an address size.
!
! Contributed by Rainer Orth  <ro@gcc.gnu.org>
!
program test_reduce
   implicit none
   integer :: i
   integer, parameter :: dp = kind(1.0_8), extent = 4

   real(dp) :: rarray(extent,extent,extent), rmat(extent,extent), &
               rvec (extent), rscl

   type :: t
      real(dp) :: field(extent)
   end type t

   type (t) :: tmat(extent, extent), tarray(extent), tscalar

   rarray = reshape ([(real(i, kind = dp), i = 1, size(rarray))], &
                     shape (rarray))

   rmat = reduce (rarray, add, dim = 1)
   if (any (rmat /= sum (rarray, 1))) stop 1

   rmat = reduce (rarray, add, dim = 2)
   if (any (rmat /= sum (rarray, 2))) stop 2

   rmat = reduce (rarray, add, dim = 3)
   if (any (rmat /= sum (rarray, 3))) stop 3

   rscl = reduce (rarray, add)
   if (rscl /= sum (rarray)) stop 4

   tmat%field(1) = rmat
   tarray = reduce (tmat, t_add, dim =1)
   rvec = reduce (rmat, add, dim = 1)
   if (any (tarray%field(1) /= rvec)) stop 5

   tscalar = reduce (tmat, t_add)
   if (tscalar%field(1) /= sum (tmat%field(1))) stop 6
contains

   pure real(dp) function add (i, j)
      real(dp), intent(in) :: i, j
      add = i + j
   end function add

   pure type(t) function t_add (i, j)
      type(t), intent(in) :: i, j
      t_add%field(1) = i%field(1) + j%field(1)
   end function t_add

end
