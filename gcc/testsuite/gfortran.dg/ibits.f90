! { dg-do run }
! Test that the mask is properly converted to the kind type of j in ibits.
program ibits_test
  implicit none
  integer(8), parameter :: n = z'00000000FFFFFFFF' ! { dg-warning "BOZ literal at .1. outside a DATA statement" }
  integer(8) i,j,k,m
  j = 1
  do i=1,70
     j = ishft(j,1) + 1
     k = ibits(j, 0, 32)
     m = iand(j,n)
     if (k /= m) STOP 1
  end do
end program ibits_test

