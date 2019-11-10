! { dg-do compile } 
! { dg-additional-options "-fcoarray=single" }

module test
contains
  subroutine oacc1
  implicit none
  integer :: i, j
  j = 0
  !$acc parallel 
  critical ! { dg-error "CRITICAL block inside of" }
    j = j + 1
  end critical
  !$acc end parallel
  !$acc serial 
  critical ! { dg-error "CRITICAL block inside of" }
    j = j + 1
  end critical
  !$acc end serial
  end subroutine oacc1

  subroutine oacc2
  implicit none
  integer :: i, j
  j = 0
  critical
    !$acc parallel ! { dg-error "OpenACC directive inside of" }
      j = j + 1
    !$acc end parallel
    !$acc serial ! { dg-error "OpenACC directive inside of" }
      j = j + 1
    !$acc end serial
  end critical
  end subroutine oacc2
end module test