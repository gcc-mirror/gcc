! Program to test the transpose intrinsic
program intrinsic_transpose
   integer, dimension (3, 3) :: a, b
   complex(kind=8), dimension (2, 2) :: c, d
   complex(kind=4), dimension (2, 2) :: e

   a = 0
   b = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = transpose (b)
   if (any (a .ne. reshape ((/1, 4, 7, 2, 5, 8, 3, 6, 9/), (/3, 3/)))) &
      STOP 1
   c = (0.0, 0.0)
   d = reshape ((/(1d0,2d0), (3d0, 4d0), (5d0, 6d0), (7d0, 8d0)/), (/2, 2/))
   c = transpose (d);
   if (any (c .ne. reshape ((/(1d0, 2d0), (5d0, 6d0), &
                              (3d0, 4d0), (7d0, 8d0)/), (/2, 2/)))) &
    STOP 1;
   
   e = reshape ((/(1.0,2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0)/), (/2, 2/))
   e = transpose (e);
   if (any (e .ne. reshape ((/(1.0, 2.0), (5.0, 6.0), &
                              (3.0, 4.0), (7.0, 8.0)/), (/2, 2/)))) &
    STOP 2;
end program
