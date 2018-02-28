!program to test nested forall construct and forall mask
program test
  implicit none
  integer a(4,4)
  integer i, j

  do i=1,4
    do j=1,4
      a(j,i) = j-i
    enddo
  enddo
  forall (i=2:4, a(1,i).GT.-2)
    forall (j=1:4, a(j,2).GT.0)
      a(j,i) = a(j,i-1)
    end forall
  end forall
  if (any (a.ne.reshape ((/0,1,2,3,-1,0,2,3,-2,-1,0,1,-3,-2,-1,0/),&
                          (/4,4/)))) STOP 1
end

