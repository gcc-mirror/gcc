! Program to test FORALL with pointer assignment inside it.
program forall_5
   type element
      integer, pointer, dimension(:)::p
   end type

   type (element) q(5)
   integer, target, dimension(25)::t

   n = 5
   do i = 1,5
      q(i)%p => t((i-1)*n + 1:i*n)
   enddo 

   forall (i = 2:5)
      q(i)%p => q(i-1)%p
   end forall

   do i = 1, 25
      t(i) = i
   enddo

   if (any(q(1)%p .ne. (/1,2,3,4,5/))) call abort
   if (any(q(2)%p .ne. (/1,2,3,4,5/))) call abort
   if (any(q(3)%p .ne. (/6,7,8,9,10/))) call abort
   if (any(q(4)%p .ne. (/11,12,13,14,15/))) call abort
   if (any(q(5)%p .ne. (/16,17,18,19,20/))) call abort
end
