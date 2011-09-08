! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/44646
!
! DO CONCURRENT
!
implicit none
integer :: i, j

outer: do, concurrent ( i = 1 : 4)
  do j = 1, 5
    if (j == 1) cycle ! OK
    cycle outer ! OK: C821   FIXME
    exit outer ! { dg-error "EXIT statement at .1. leaves DO CONCURRENT construct" }
  end do
end do outer

do concurrent (j = 1:5)
  cycle ! OK
end do

outer2: do j = 1, 7
  do concurrent (j=1:5:2) ! cycle outer2 - bad: C821
    cycle outer2 ! { dg-error "leaves DO CONCURRENT construct" }
  end do
end do outer2

do concurrent ( i = 1 : 4)
  exit ! { dg-error "EXIT statement at .1. leaves DO CONCURRENT construct" }
end do
end

subroutine foo()
  do concurrent ( i = 1 : 4)
    return   ! { dg-error "Image control statement RETURN" }
    sync all ! { dg-error "Image control statement SYNC" }
    call test () ! { dg-error "Subroutine call to .test. in DO CONCURRENT block at .1. is not PURE" }
    stop ! { dg-error "Image control statement STOP" }
  end do
  do concurrent ( i = 1 : 4)
    critical ! { dg-error "Image control statement CRITICAL at .1. in DO CONCURRENT block" }
      print *, i
!    end critical
  end do

  critical
    do concurrent ( i = 1 : 4) ! OK
    end do
  end critical
end

subroutine caf()
  use iso_fortran_env
  implicit none
  type(lock_type), allocatable :: lock[:]
  integer :: i
  do, concurrent (i = 1:3)
    allocate (lock[*]) ! { dg-error "ALLOCATE of coarray at .1. in DO CONCURRENT block" }
    lock(lock) ! { dg-error "Image control statement LOCK" }
    unlock(lock) ! { dg-error "Image control statement UNLOCK" }
    deallocate (lock) ! { dg-error "DEALLOCATE of coarray at .1. in DO CONCURRENT block" }
  end do

  critical
    allocate (lock[*]) ! { dg-error "ALLOCATE of coarray at .1. in CRITICAL block" }
    lock(lock) ! { dg-error "Image control statement LOCK" }
    unlock(lock) ! { dg-error "Image control statement UNLOCK" }
    deallocate (lock) ! { dg-error "DEALLOCATE of coarray at .1. in CRITICAL block" }
  end critical
end subroutine caf
