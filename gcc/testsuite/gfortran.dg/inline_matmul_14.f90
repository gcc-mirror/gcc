! { dg-do run }
! { dg-options "-O -ffrontend-optimize -fdump-tree-optimized" }
! PR 79930 - missed optimization by not inlining matmul in expressions.

module foo
  implicit none
contains
  subroutine test1
    ! Test with fixed dimensions
    real, dimension(3,2) :: a1
    real, dimension(2,4) :: b1
    real, dimension(3,4) :: cres1
    real, dimension(3,3) :: a2
    real, dimension(3) :: v1, v2
    real :: r
    character(len=9*18) :: r1, r2
    real(kind=8), dimension(3,3) :: a3, b3, c3, d3, res3
    
    data a1 / 2.,  -3.,  5.,  -7., 11., -13./
    data b1 /17., -23., 29., -31., 37., -39., 41., -47./
    data cres1 /195., -304.,  384.,  275., -428.,  548.,  347., -540.,  692.,  411., -640.,  816./

    data a2 / 2.,  -3.,  5.,  -7., 11., -13., 17., -23., 29./
    data v1 /-31., 37., -41./
    data v2 /43., -47., 53./

    data a3/-2.d0, 3.d0, 5.d0, -7.d0, -11.d0, 13.d0, 17.d0, -19.d0, -23.d0/
    data b3/29.d0, -31.d0, 37.d0, -41.d0, 43.d0, -47.d0, 53.d0, -59.d0, 61.d0/
    data c3/-67.d0,71.d0, 73.d0, -79.d0, -83.d0, 89.d0, 97.d0, -101.d0, 103.d0/
    data d3/107.d0, 109.d0, 113.d0, 127.d0, 131.d0, 137.d0, 139.d0, 149.d0, 151.d0/
    data res3/48476106.d0, -12727087.d0, -68646789.d0, 58682206.d0, -15428737.d0, -83096539.d0,&
         & 65359710.d0, -17176589.d0, -92551887.d0/

    write (unit=r1, fmt='(12F12.5)') matmul(a1,b1)
    write (unit=r2, fmt='(12F12.5)') cres1
    if (r1 /= r2) STOP 1

    r = dot_product(matmul(a2,v1),v2)
    if (abs(r+208320) > 1) STOP 2

    write (unit=r1,fmt='(1P,9E18.10)') matmul(matmul(a3,b3),matmul(c3,d3))
    write (unit=r2,fmt='(1P,9E18.10)') res3
    if (r1 /= r2) STOP 3
    
  end subroutine test1

  subroutine test2
    ! Test with dimensions not known at compile-time
    real, dimension(:,:), allocatable :: a1
    real, dimension(:,:), allocatable :: b1
    real, dimension(3,4) :: cres1
    real, dimension(:,:), allocatable :: a2
    real, dimension(:), allocatable :: v1, v2
    real :: r
    character(len=9*18) :: r1, r2
    real(kind=8), dimension(3,3) :: a3, b3, c3, d3, res3
    data cres1 /195., -304.,  384.,  275., -428.,  548.,  347., -540.,  692.,  411., -640.,  816./
    data res3/48476106.d0, -12727087.d0, -68646789.d0, 58682206.d0, -15428737.d0, -83096539.d0,&
         & 65359710.d0, -17176589.d0, -92551887.d0/
    
    a1 = reshape([ 2.,  -3.,  5.,  -7., 11., -13.], [3,2])
    b1 = reshape([17., -23., 29., -31., 37., -39., 41., -47.],[2,4])

    a2 = reshape([2.,  -3.,  5.,  -7., 11., -13., 17., -23., 29.],[3,3]);
    v1 = [-31., 37., -41.]
    v2 = [43., -47., 53.]

    a3 = reshape([-2.d0, 3.d0, 5.d0, -7.d0, -11.d0, 13.d0, 17.d0, -19.d0, -23.d0], [3,3])
    b3 = reshape([29.d0, -31.d0, 37.d0, -41.d0, 43.d0, -47.d0, 53.d0, -59.d0, 61.d0], [3,3])
    c3 = reshape([-67.d0,71.d0, 73.d0, -79.d0, -83.d0, 89.d0, 97.d0, -101.d0, 103.d0], [3,3])
    d3 = reshape([107.d0, 109.d0, 113.d0, 127.d0, 131.d0, 137.d0, 139.d0, 149.d0, 151.d0],[3,3])

    write (unit=r1, fmt='(12F12.5)') matmul(a1,b1)
    write (unit=r2, fmt='(12F12.5)') cres1
    if (r1 /= r2) STOP 4

    r = dot_product(matmul(a2,v1),v2)
    if (abs(r+208320) > 1) STOP 5

    write (unit=r1,fmt='(1P,9E18.10)') matmul(matmul(a3,b3),matmul(c3,d3))
    write (unit=r2,fmt='(1P,9E18.10)') res3
    if (r1 /= r2) STOP 6
    
  end subroutine test2

end module foo

program main
  use foo
  implicit none
  call test1
  call test2
!  call test3
end program main
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "optimized" } }
