! Program to test WHERE on unknown size arrays
program where_3
   integer A(10, 2)

   A = 0
   call sub(A)

contains

subroutine sub(B)
   integer, dimension(:, :) :: B

   B(1:5, 1) = 0
   B(6:10, 1) = 5
   where (B(:,1)>0)
      B(:,1) = B(:,1) + 10
   endwhere
   if (any (B .ne. reshape ((/0, 0, 0, 0, 0, 15, 15, 15, 15, 15, &
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0/), (/10, 2/)))) call abort
end subroutine
end program
