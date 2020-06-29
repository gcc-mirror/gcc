! Program to test FORALL with pointer assignment inside it.
program forall_5
   type element
      integer, pointer, dimension(:)::p
   end type

   type (element) :: q(5), r(5)
   integer, target, dimension(25)::t

   n = 5
   do i = 1,5
      r(i)%p => t((i-1)*n + 1:i*n)
   enddo 

   forall (i = 2:5)
      q(i)%p => r(i-1)%p
   end forall

   do i = 1, 25
      t(i) = i
   enddo

   if (any(r(1)%p .ne. (/1,2,3,4,5/))) STOP 1
   if (any(q(2)%p .ne. (/1,2,3,4,5/))) STOP 2
   if (any(q(3)%p .ne. (/6,7,8,9,10/))) STOP 3
   if (any(q(4)%p .ne. (/11,12,13,14,15/))) STOP 4
   if (any(q(5)%p .ne. (/16,17,18,19,20/))) STOP 5
end
