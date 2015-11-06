! { dg-do compile } 
! <http://news.gmane.org/find-root.php?message_id=%3C563B78B5.5090506%40acm.org%3E>
! { dg-xfail-if "TODO" { *-*-* } }

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
