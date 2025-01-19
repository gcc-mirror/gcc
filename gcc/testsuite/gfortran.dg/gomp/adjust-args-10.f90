! { dg-additional-options "-fdump-tree-gimple" }

! This mainly checks 'has_device_addr' without associated 'need_device_addr'
!
! Do diagnostic check / dump check only;
! Note: this test should work as run-test as well.

module m
  use iso_c_binding
  ! use omp_lib
  implicit none (type, external)
  interface
    integer function omp_get_default_device (); end
    integer function omp_get_num_devices (); end
  end interface

contains
  subroutine g (x, y)
    !$omp declare variant(f) adjust_args(need_device_ptr: x, y) match(construct={dispatch})
    type(c_ptr), value :: x, y
  end

  subroutine f (cfrom, cto)
    type(c_ptr), value :: cfrom, cto
    integer, save :: cnt = 0
    cnt = cnt + 1
    if (cnt >= 3) then
      if (omp_get_default_device () /= -1  &
          .and. omp_get_default_device () < omp_get_num_devices ()) then
        ! On offload device but not mapped
        if (.not. c_associated(cfrom)) & ! Not mapped
          stop 1
      else
        block
          integer, pointer :: from(:)
          call c_f_pointer(cfrom, from, shape=[1])
          if (from(1) /= 5) &
            stop 2
        end block
      end if
      return
    end if

    !$omp target is_device_ptr(cfrom, cto)
      block
        integer, pointer :: from(:), to(:)
        call c_f_pointer(cfrom, from, shape=[2])
        call c_f_pointer(cto, to, shape=[2])
        to(1) = from(1) * 10
        to(2) = from(2) * 10
      end block
  end

  subroutine sub (a, b)
    integer, target :: a(:), b(:)
    type(c_ptr), target :: ca, cb

    ca = c_loc(a)
    cb = c_loc(b)

    ! The has_device_addr is a bit questionable as the caller is not actually
    ! passing a device address - but we cannot pass one because of the
    ! following:
    !
    ! As for 'b' need_device_ptr has been specified and 'b' is not
    ! in the semantic requirement set 'is_device_ptr' (and only in 'has_device_addr')
    ! "the argument is converted in the same manner that a use_device_ptr clause
    !  on a target_data construct converts its pointer"

    !$omp dispatch is_device_ptr(ca), has_device_addr(cb)
      call g (ca, cb)  ! { dg-warning "'has_device_addr' for 'cb' does not imply 'is_device_ptr' required for 'need_device_ptr' \\\[-Wopenmp\\\]" }
  end
end

program main
  use m
  implicit none (type, external)

  integer, target :: A(2), B(2) = [123, 456], C(1) = [5]
  integer, pointer :: p(:)

  p => A

  !$omp target enter data map(A, B)

  ! Note: We don't add  'use_device_addr(B)' here;
  ! if we do, it will fail with an illegal memory access (why?).
  !$omp target data use_device_ptr(p)
    call sub(p, B)
    call sub(C, B)  ! C is not mapped -> 'from' ptr == NULL
  !$omp end target data

  !$omp target exit data map(A, B)
end

! { dg-final { scan-tree-dump-times "#pragma omp dispatch is_device_ptr\\(ca\\) has_device_addr\\(cb\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(cb" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "f \\(ca\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }
