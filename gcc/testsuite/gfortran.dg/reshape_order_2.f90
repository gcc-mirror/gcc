! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Value 3 out of range in ORDER argument to RESHAPE intrinsic" }
program main
  implicit none
  integer(kind=1), dimension(6) :: source1 = (/ 1, 2, 3, 4, 5, 6 /)
  integer, dimension(2) :: shape1 = (/ 2, 3/)
  integer(kind=1), dimension(2) :: pad1 = (/ 0, 0/)
  character(len=200) :: l1, l2
  integer :: i1, i2

  l1 = "3 2"
  read(unit=l1,fmt=*) i1, i2
  write (unit=l2,fmt=*) reshape(source1, shape1, pad1, (/i1, i2/)) ! Invalid
end program main
! { dg-output "Fortran runtime error: Value 3 out of range in ORDER argument to RESHAPE intrinsic" }
