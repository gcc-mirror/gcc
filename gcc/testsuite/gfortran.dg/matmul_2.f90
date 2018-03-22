!{ dg-do run }
! PR libfortran/26985
program matmul_2
  implicit none
  integer :: a(2,9), b(9,7), c(2,7)
  integer :: i, j

  a = 1
  b = 2
  c = 1789789
  c(:,1:7:2) = matmul(a,b(:,1:7:2))

  if (c(1,1) /= 18 .or. c(2,1) /= 18 .or. &
      c(1,2) /= 1789789 .or. c(2,2) /= 1789789 .or. &
      c(1,3) /= 18 .or. c(2,3) /= 18 .or. &
      c(1,4) /= 1789789 .or. c(2,4) /= 1789789 .or. &
      c(1,5) /= 18 .or. c(2,5) /= 18 .or. &
      c(1,6) /= 1789789 .or. c(2,6) /= 1789789 .or. &
      c(1,7) /= 18 .or. c(2,7) /= 18) STOP 1
      
end program matmul_2
