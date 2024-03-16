! See also 'if.f95'.

! { dg-do compile } 

program test
  implicit none

  logical :: x
  integer :: i

  !$acc parallel self () ! { dg-error "Invalid character" }
  !$acc parallel self (i) ! { dg-error "scalar LOGICAL expression" }
  !$acc end parallel 
  !$acc parallel self (1) ! { dg-error "scalar LOGICAL expression" }
  !$acc end parallel 

  !$acc kernels self () ! { dg-error "Invalid character" }
  !$acc kernels self (i) ! { dg-error "scalar LOGICAL expression" }
  !$acc end kernels 
  !$acc kernels self (1) ! { dg-error "scalar LOGICAL expression" }
  !$acc end kernels

  !$acc serial self () ! { dg-error "Invalid character" }
  !$acc serial self (i) ! { dg-error "scalar LOGICAL expression" }
  !$acc end serial
  !$acc serial self (1) ! { dg-error "scalar LOGICAL expression" }
  !$acc end serial
  
  ! at most one self clause may appear
  !$acc parallel self (.false.) self (.false.) { dg-error "Duplicated 'self' clause" }
  !$acc kernels self (.false.) self (.false.) { dg-error "Duplicated 'self' clause" }
  !$acc serial self (.false.) self (.false.) { dg-error "Duplicated 'self' clause" }

  !$acc parallel self
  !$acc end parallel 
  !$acc parallel self (x)
  !$acc end parallel
  !$acc parallel self (.true.)
  !$acc end parallel
  !$acc parallel self (i.gt.1)
  !$acc end parallel

  !$acc kernels self
  !$acc end kernels 
  !$acc kernels self (x)
  !$acc end kernels
  !$acc kernels self (.true.)
  !$acc end kernels
  !$acc kernels self (i.gt.1)
  !$acc end kernels

  !$acc serial self
  !$acc end serial
  !$acc serial self (x)
  !$acc end serial
  !$acc serial self (.true.)
  !$acc end serial
  !$acc serial self (i.gt.1)
  !$acc end serial

end program test
