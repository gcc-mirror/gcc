! Program to test nested forall
program forall2
  implicit none
  integer a(4,4,2)
  integer i, j, k, n

  a(:,:,1) = reshape((/ 1, 2, 3, 4,&
                        5, 6, 7, 8,&
                        9,10,11,12,&
                       13,14,15,16/), (/4,4/))
  a(:,:,2) = a(:,:,1) + 16
  n=4
  k=1
  ! Mirror half the matrix
  forall (i=k:n)
   forall (j=1:5-i)
     a(i,j,:) = a(j,i,:)
   end forall
  end forall

  if (any (a(:,:,1) & 
      .ne. reshape((/ 1, 5, 9,13,&
                      2, 6,10, 8,&
                      3, 7,11,12,&
                      4,14,15,16/),(/4,4/)))) STOP 1
  if (any (a(:,:,2) .ne. a(:,:,1) + 16)) STOP 2
end
