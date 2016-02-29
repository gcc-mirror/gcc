! { dg-do run }
! { dg-additional-options "-Ofast -fno-inline" }

subroutine check (a, b)
   real *8, intent(in) :: a(4), b(4)

   IF (abs(a(1)-b(1)) > 1) THEN
      CALL ABORT
   END IF
end subroutine check

program main
  real *8 :: mu(4,26), mumax(4), mumax2(4)

  integer :: i, k

  do k=1,26
     do i=1,4
        mu(i, k) = 4*(i-1) + k
     end do
  end do

  mumax = 0; 
  do k=1,26
     do i=1,3
        mumax(i) = max(mumax(i), mu(i,k)+mu(i,k))
     end do
  end do

  mumax2 = 0;
  do i=1,3
     do k=1,26
        mumax2(i) = max(mumax2(i), mu(i,k)+mu(i,k))
     end do
  end do

  CALL check (mumax, mumax2)

  return
end program
