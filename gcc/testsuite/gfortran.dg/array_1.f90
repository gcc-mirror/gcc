! { dg-do run }
! PR 15553 : the array used to be filled with garbage
! this problem disappeared between 2004-05-20 and 2004-09-15
program arrpack
  implicit none
  
  double precision x(10,10)
  integer i, j

  x = -1
  do i=1,6
     do j=1,5
        x(i,j) = i+j*10
     end do
  end do
  call pack (x, 6, 5)

  if (any(reshape(x(1:10,1:3), (/ 30 /)) & 
          /= (/ 11, 12, 13, 14, 15, 16,  &
                21, 22, 23, 24, 25, 26,  &
                31, 32, 33, 34, 35, 36,  &
                41, 42, 43, 44, 45, 46,  &
                51, 52, 53, 54, 55, 56 /))) STOP 1
  
contains
  
  subroutine pack (arr, ni, nj)
    integer, intent(in) :: ni, nj
    double precision, intent(inout) :: arr(:,:)
    double precision :: tmp(ni,nj)
    tmp(:,:) = arr(1:ni, 1:nj)
    call copy (arr, tmp, ni, nj)
  end subroutine pack
  
  subroutine copy (dst, src, ni, nj)
    integer, intent(in) :: ni, nj
    double precision, intent(out) :: dst(ni, nj)
    double precision, intent(in)  :: src(ni, nj)
    dst = src
  end subroutine copy
  
end program arrpack
