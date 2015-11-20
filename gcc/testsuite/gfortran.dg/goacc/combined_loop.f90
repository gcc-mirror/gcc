! { dg-do compile } 

!
! PR fortran/64726
!
subroutine oacc1()
  implicit none
  integer :: i
  integer  :: a
  !$acc parallel loop reduction(+:a)
  do i = 1,5
  enddo
  !$acc end parallel loop
  !$acc kernels loop collapse(2)
  do i = 2,6
     do a = 3,5
     enddo
  enddo
  !$acc end kernels loop
end subroutine oacc1
