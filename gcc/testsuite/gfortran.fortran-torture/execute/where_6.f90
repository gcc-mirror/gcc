! Program to test WHERE inside FORALL and the WHERE assignment need temporary
program where_6
   integer :: A(5,5)

   A(1,:) = (/1,0,0,0,0/)
   A(2,:) = (/2,1,1,1,0/)
   A(3,:) = (/1,2,2,0,2/)
   A(4,:) = (/2,1,0,2,3/)
   A(5,:) = (/1,0,0,0,0/)

   ! Where inside FORALL.
   ! WHERE masks must be evaluated before executing the assignments
   m=5
   forall (I=1:4)
      where (A(I,:) .EQ. 0)
         A(1:m,I) = A(1:m,I+1) + I
      elsewhere (A(I,:) >2)
         A(I,1:m) = 6
      endwhere
   end forall
   if (any (A .ne. reshape ((/1,2,6,2,1,0,1,2,1,2,0,1,2,5,0,0,1,6,2,0,0,0,2,&
                             6,0/), (/5, 5/)))) call abort
end
