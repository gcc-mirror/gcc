! Program to test WHERE inside FORALL
program where_1
   integer :: A(5,5)

   A(1,:) = (/1,0,0,0,0/)
   A(2,:) = (/2,1,1,1,0/)
   A(3,:) = (/1,2,2,0,2/)
   A(4,:) = (/2,1,0,2,3/)
   A(5,:) = (/1,0,0,0,0/)

   ! Where inside FORALL.
   ! WHERE masks must be evaluated before executing the assignments
   forall (I=1:5)
      where (A(I,:) .EQ. 0)
         A(:,I) = I
      elsewhere (A(I,:) >2)
         A(I,:) = 6
      endwhere
   end forall

   if (any (A .ne. reshape ((/1, 1, 1, 1, 1, 0, 1, 2, 1, 2, 0, 1, 2, 3, 0, &
      0, 1, 4, 2, 0, 0, 5, 6, 6, 5/), (/5, 5/)))) call abort

   ! Where inside DO
   A(1,:) = (/1,0,0,0,0/)
   A(2,:) = (/2,1,1,1,0/)
   A(3,:) = (/1,2,2,0,2/)
   A(4,:) = (/2,1,0,2,3/)
   A(5,:) = (/1,0,0,0,0/)

   do I=1,5
      where (A(I,:) .EQ. 0)
         A(:,I) = I
      elsewhere (A(I,:) >2)
         A(I,:) = 6
      endwhere
   enddo

   if (any (A .ne. reshape ((/1, 1, 1, 1, 1, 0, 1, 2, 1, 2, 0, 1, 2, 6, 0, &
      0, 1, 0, 2, 0, 0, 0, 5, 5, 5/), (/5, 5/)))) call abort
end
