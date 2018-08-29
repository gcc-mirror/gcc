! { dg-do  run }
! Test CSHIFT with array argument for shift
module rnd
  implicit none
contains
  subroutine fill(a,n)
    integer, intent(out), dimension(:,:) :: a
    integer, intent(in) :: n
    real, dimension(size(a,1),size(a,2)) :: r
    call random_number(r)
    a = int(2*n*r-n)
  end subroutine fill
end module rnd

module csh
  implicit none
contains
  subroutine emul_cshift(a,sh_in,dim, c)
    integer, dimension(:,:,:), intent(in) :: a
    integer, dimension(:,:,:), intent(out) :: c
    integer, dimension(:,:), intent(in) :: sh_in
    integer, intent(in) :: dim
    integer :: sh, rsh
    integer :: s1, s2, s3, n, i
    integer :: n1, n2, n3
    n1 = size(a,1)
    n2 = size(a,2)
    n3 = size(a,3)
    if (dim == 1) then
       n = n1
       do s2=1,n2
          do s3=1,n3
             sh = modulo(sh_in(s2,s3), n)
             rsh = n - sh
             do i=1,rsh
                c(i,s2,s3) = a(i+sh,s2,s3)
             end do
             do i=rsh+1,n
                c(i,s2,s3) = a(i-rsh,s2,s3)
             end do
          end do
       end do
    else if (dim == 2) then
       n = n2
       do s3=1,n3
          do s1=1,n1
             sh = modulo(sh_in(s1,s3),n)
             rsh = n - sh
             do i=1,rsh
                c(s1,i,s3) = a(s1,i+sh,s3)
             end do
             do i=rsh+1,n
                c(s1,i,s3) = a(s1,i-rsh,s3)
             end do
          end do
       end do

    else if (dim == 3) then
       n = n3
       do s2=1,n2
          do s1=1,n1
             sh = modulo(sh_in(s1,s2),n)
             rsh = n - sh
             do i=1,rsh
                c(s1,s2,i) = a(s1,s2,i+sh)
             end do
             do i=rsh+1,n
                c(s1,s2,i) = a(s1,s2,i-rsh)
             end do
          end do
       end do
    else
       stop "Illegal dim"
    end if
  end subroutine emul_cshift
end module csh
program main
  use csh
  use rnd
  implicit none
  integer, parameter :: n1=30,n2=40,n3=50
  integer, dimension(n1,n2,n3) :: a, b,c
  integer :: s1, s2, s3
  integer :: dim
  integer, dimension(:,:), allocatable :: sh1, sh2, sh3
  integer, dimension(:), allocatable :: sh_shift
  integer :: sh, rsh
  integer :: i,j,k,v
  type t
     integer :: i1, i2, i3
  end type t
  type(t), dimension(n1,n2,n3) :: ta, tb

  v = 1
  do k=1,n3
     do j=1,n2
        do i=1,n1
           a(i,j,k) = v
           v = v + 1
        end do
     end do
  end do

  ta%i1 = a
  ta%i2 = a+a
  ta%i3 = a+a+a
  allocate(sh1(n2,n3))
  allocate(sh2(n1,n3))
  allocate(sh3(n1,n2))

  call fill(sh1,10)
  call fill(sh2,10)
  call fill(sh3,10)

  b = cshift(a,sh1,1)
  call emul_cshift(a,sh1,1,c)
  if (any(b /= c)) then
     print *,b
     print *,c
     STOP 1
  end if
  tb = cshift(ta,sh1,1)
  if (any(tb%i1 /= c)) STOP 2
  
  b = cshift(a,sh2,2)
  call emul_cshift(a,sh2,2,c)
  if (any(b /= c)) STOP 3
  tb = cshift(ta,sh2,2)
  if (any (tb%i2 /= c*2)) STOP 4

  b = cshift(a,sh3,3)
  call emul_cshift(a,sh3,3,c)
  if (any(b /= c)) STOP 5
  tb = cshift(ta,sh3,3)
  if (any(tb%i3 /= c*3)) STOP 6

  b = -42
  c = -42
  b(1:n1:2,:,:) = cshift(a(1:n1/2,:,:),sh1,1)
  call emul_cshift(a(1:n1/2,:,:), sh1, 1, c(1:n1:2,:,:))
  if (any(b /= c)) STOP 7

  tb%i1 = -42
  tb%i2 = -2*42
  tb%i3 = -3*42
  tb(1:n1:2,:,:) = cshift(ta(1:n1/2,:,:),sh1,1)
  if (any(tb%i1 /= b)) STOP 8
  if (any(tb%i2 /= 2*b)) STOP 9
  if (any(tb%i3 /= 3*b)) STOP 10
  
9000 format (99(3(I3,1X),2X))
end program main
