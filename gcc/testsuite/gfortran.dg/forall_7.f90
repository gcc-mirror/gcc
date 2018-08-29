! { dg-do run }
  integer :: a(10,10)
  integer :: tot
  a(:,:) = 0
  forall (i = 1:10)
    forall (j = 1:10)
      a(i,j) = 1
    end forall
    forall (k = 1:10)
      a(i,k) = a(i,k) + 1
    end forall
  end forall
  tot = sum(a(:,:))
! print *, tot
  if (tot .ne. 200) STOP 1
end
