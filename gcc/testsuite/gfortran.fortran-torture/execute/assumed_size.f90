! Program to test assumed size arrays
subroutine test2(p)
   integer, dimension(2, *) :: p

   if (any (p(:, 1:3) .ne. reshape((/1, 2, 4, 5, 7, 8/), (/2, 3/)))) &
     STOP 1
end subroutine

program assumed_size
   integer, dimension (3, 3) :: a
   external test2

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))

   call test1(a, (/1, 2, 3, 4, 5, 6/))
   if (a(1,1) .ne. 0) STOP 1
   a(1, 1) = 1
   call test1(a(1:2, :), (/1, 2, 4, 5, 7, 8/))
   if (a(1,1) .ne. 0) STOP 2
   a(1, 1) = 1
   call test1(a(3:1:-1, :), (/3, 2, 1, 6, 5, 4/))
   if (a(3,1) .ne. 0) STOP 3
   a(3, 1) = 3
   call test1(a(:, 2:3), (/4, 5, 6, 7, 8, 9/))
   if (a(1, 2) .ne. 0) STOP 4
   a(1, 2) = 4
   
   call test2(a(1:2, :))
   call test2((/1, 2, 4, 5, 7, 8/))
contains
subroutine test1(p, q)
   integer, dimension(*) :: p
   integer, dimension(1:) :: q

   if (any (p(1:size(q)) .ne. q)) STOP 2
   p(1) = 0
end subroutine

end program
