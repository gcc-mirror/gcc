! { dg-do run }
!
! Verify that the bounds are correctly set when assigning pointers.
!
! PR fortran/33139
!
program prog
  implicit none
  real, target :: a(-10:10)
  real, pointer :: p(:),p2(:)
  integer :: i
  do i = -10, 10
    a(i) = real(i)
  end do
  p  => a
  p2 => p
  if((lbound(p, dim=1) /= -10) .or. (ubound(p, dim=1) /= 10)) &
    STOP 1
  if((lbound(p2,dim=1) /= -10) .or. (ubound(p2,dim=1) /= 10)) &
    STOP 2
  do i = -10, 10
    if(p(i) /= real(i)) STOP 3
    if(p2(i) /= real(i)) STOP 4
  end do
  p => a(:)
  p2 => p
  if((lbound(p, dim=1) /= 1) .or. (ubound(p, dim=1) /= 21)) &
    STOP 5
  if((lbound(p2,dim=1) /= 1) .or. (ubound(p2,dim=1) /= 21)) &
    STOP 6
  p2 => p(:)
  if((lbound(p2,dim=1) /= 1) .or. (ubound(p2,dim=1) /= 21)) &
    STOP 7
  call multdim()
contains
  subroutine multdim()
    real, target, allocatable :: b(:,:,:)
    real, pointer :: ptr(:,:,:)
    integer :: i, j, k
    allocate(b(-5:5,10:20,0:3))
    do i = 0, 3
      do j = 10, 20
        do k = -5, 5
          b(k,j,i) = real(i+10*j+100*k)
        end do
      end do
    end do
    ptr => b
    if((lbound(ptr,dim=1) /= -5) .or. (ubound(ptr,dim=1) /=  5) .or. &
       (lbound(ptr,dim=2) /= 10) .or. (ubound(ptr,dim=2) /= 20) .or. &
       (lbound(ptr,dim=3) /=  0) .or. (ubound(ptr,dim=3) /=  3))     &
      STOP 8
    do i = 0, 3
      do j = 10, 20
        do k = -5, 5
          if(ptr(k,j,i) /= real(i+10*j+100*k)) STOP 9
        end do
      end do
    end do
    ptr => b(:,:,:)
    if((lbound(ptr,dim=1) /= 1) .or. (ubound(ptr,dim=1) /= 11) .or. &
       (lbound(ptr,dim=2) /= 1) .or. (ubound(ptr,dim=2) /= 11) .or. &
       (lbound(ptr,dim=3) /= 1) .or. (ubound(ptr,dim=3) /=  4))     &
      STOP 10
  end subroutine multdim
end program prog
