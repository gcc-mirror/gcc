! { dg-do compile }
! { dg-options "-Warray-temporaries" }

subroutine t1(n1,n2, gfft, ufft)
  implicit none
  integer :: n1, n2, i
  real :: gfft(n1,n2), ufft(n2)
  DO i=1, n1
     gfft(i,:)=gfft(i,:)*ufft(i)
  END DO
end subroutine t1
