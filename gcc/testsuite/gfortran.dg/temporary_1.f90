! { dg-do run }
! PR 27662. Don't zero the first stride to indicate a temporary. It
! may be used later.
program pr27662
 implicit none
 real(kind=kind(1.0d0)), dimension (2, 2):: x, y, z;
 integer i, j
 x(1,1) = 1.d0 
 x(2,1) = 0.d0
 x(1,2) = 0.d0
 x(2,2) = 1.d0 
 z = matmul (x, transpose (test ()))
 do i = 1, size (x, 1)
   do j = 1, size (x, 2)
     if (x (i, j) .ne. z (i, j)) STOP 1
   end do
 end do

contains
 function test () result (res)
   real(kind=kind(1.0d0)), dimension(2,2) :: res
   res(1,1) = 1.d0 
   res(2,1) = 0.d0
   res(1,2) = 0.d0
   res(2,2) = 1.d0 
 end function
end
