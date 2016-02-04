program test
  implicit none
  integer a(10), i

  a(:) = 0
  
  ! Array reductions.
  
  !$acc parallel reduction (+:a) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction (+:a) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc kernels
  !$acc loop reduction (+:a) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a = a + 1
  end do
  !$acc end kernels

  ! Subarray reductions.
  
  !$acc parallel reduction (+:a(1:5)) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction (+:a(1:5)) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc kernels
  !$acc loop reduction (+:a(1:5)) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a = a + 1
  end do
  !$acc end kernels

  ! Reductions on array elements.
  
  !$acc parallel reduction (+:a(1)) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction (+:a(1)) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end parallel

  !$acc kernels
  !$acc loop reduction (+:a(1)) ! { dg-error "Array 'a' is not permitted in reduction" }
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end kernels
  
  print *, a
end program test
