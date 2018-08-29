! { dg-do run }
! { dg-options "-O" }
! PR 80304 - this used to give a wrong result.
! Original test case by Chinoune
module test_mod
  implicit none

contains

  pure real function add(i,j,k)
    integer ,intent(in) :: i,j,k
    add = real(i+j+k)+1.
  end function add

  pure real function add2(i,j,k)
    integer ,intent(in) :: i,j,k
    add2 = real(i+j+k)
  end function add2

  subroutine check_err(a, s)
    real, dimension(:,:), intent(in) :: a
    real, intent(in) :: s
    if (abs(sum(a) - s) > 1e-5) STOP 1
  end subroutine check_err

end module test_mod

program test 
  use test_mod
  implicit none

  integer :: i ,j
  real :: a(0:1,0:1) ,b(0:1,0:1)

  ! first do-concurrent loop  
  a = 0.
  b = 0.
  DO CONCURRENT( i=0:1 ,j=0:1)
     a(i,j) = add(i,j,abs(i-j))
     b(i,j) = add2(i,j,abs(i-j))
  END DO
  call check_err (a, 10.)
  call check_err (b, 6.)

  ! normal do loop  
  a = 0.
  b = 0.
  DO i=0,1 
     DO j=0,1
        a(i,j) = add(i,j,abs(i-j))
        b(i,j) = add2(i,j,abs(i-j))
     END DO
  END DO
  call check_err (a, 10.)
  call check_err (b, 6.)

  ! second do-concuurent loop  
  a = 0.
  b = 0.
  DO CONCURRENT( i=0:1 ,j=0:1)
     a(i,j) = add(i,j,abs(i-j))
     b(i,j) = add2(i,j,abs(i-j))
  END DO
  call check_err (a, 10.)
  call check_err (b, 6.)

end program test
