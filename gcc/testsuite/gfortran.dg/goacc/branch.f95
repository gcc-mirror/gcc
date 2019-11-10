! { dg-do compile } 

program test
  implicit none

  integer :: i

  if (.true.) then
    !$acc parallel 
  end if ! { dg-error "Unexpected" }
  !$acc end parallel 
  end if

  if (.true.) then
    !$acc kernels 
  end if ! { dg-error "Unexpected" }
  !$acc end kernels 
  end if

  if (.true.) then
    !$acc serial 
  end if ! { dg-error "Unexpected" }
  !$acc end serial 
  end if

  !$acc parallel
  if (.true.) then
    !$acc end parallel ! { dg-error "Unexpected" }
  end if 
  !$acc end parallel

  !$acc kernels
  if (.true.) then
    !$acc end kernels ! { dg-error "Unexpected" }
  end if 
  !$acc end kernels

  !$acc serial
  if (.true.) then
    !$acc end serial ! { dg-error "Unexpected" }
  end if 
  !$acc end serial

  !$acc parallel
  if (.true.) then
  end if
  !$acc end parallel

  !$acc kernels
  if (.true.) then
  end if
  !$acc end kernels

  !$acc serial
  if (.true.) then
  end if
  !$acc end serial

  if (.true.) then
    !$acc parallel
    !$acc end parallel
  end if

  if (.true.) then
    !$acc kernels
    !$acc end kernels
  end if

  if (.true.) then
    !$acc serial
    !$acc end serial
  end if

end program test 