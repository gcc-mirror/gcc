! Test invalid usages of the if_present clause.

subroutine t1
  implicit none
  !$acc routine gang if_present ! { dg-error "Failed to match clause" }
  integer a, b, c(10)
  real, allocatable :: x, y, z(:)

  a = 5
  b = 10
  c(:) = -1

  allocate (x, y, z(100))

  !$acc enter data copyin(a) if_present ! { dg-error "Failed to match clause" }
  !$acc exit data copyout(a) if_present ! { dg-error "Failed to match clause" }

  !$acc data copy(a) if_present ! { dg-error "Failed to match clause" }
  !$acc end data ! { dg-error "Unexpected ..ACC END DATA statement" }

  !$acc declare link(a) if_present ! { dg-error "Unexpected junk after" }

  !$acc init if_present ! { dg-error "Unclassifiable OpenACC directive" }
  !$acc shutdown if_present ! { dg-error "Unclassifiable OpenACC directive" }
  
  !$acc update self(a) device_type(nvidia) device(b) if_present ! { dg-error "Failed to match clause" }
end subroutine t1

subroutine t2
  implicit none
  integer a, b, c(10)

  a = 5
  b = 10
  c(:) = -1

  !$acc parallel
  !$acc loop if_present ! { dg-error "Failed to match clause" }
  do b = 1, 10
  end do
  !$acc end parallel

  !$acc kernels loop if_present ! { dg-error "Failed to match clause" }
  do b = 1, 10
  end do
  !$acc end kernels loop ! { dg-error "Unexpected ..ACC END KERNELS LOOP statement" }

  !$acc parallel loop if_present ! { dg-error "Failed to match clause" }
  do b = 1, 10
  end do
  !$acc end parallel loop   ! { dg-error "Unexpected ..ACC END PARALLEL LOOP statement" }
end subroutine t2
