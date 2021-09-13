! { dg-do compile }
! { dg-prune-output "compilation terminated." }
!
! FIRSTPRIVATE + class array
!
! For now: Expected to give "Sorry" for polymorphic arrays.
!
! Polymorphic arrays are tricky - at least if not allocatable, they become:
!   var.0 = var._data.data
! which needs to be handled properly.
!
!
program select_type_openmp
  use iso_c_binding
  !use omp_lib
  implicit none
  integer :: i
  integer :: A(4)
  type(c_ptr) :: B(4)

  B = [(c_null_ptr, i=1,4)]
  A = [1,2,3,4]
  call sub(A, B)
contains
  subroutine sub(val1, val2)
    class(*) :: val1(4)
    type(c_ptr) :: val2(2:5)

    !$OMP PARALLEL firstprivate(val2)
      do i = 2, 5
        if (c_associated (val2(i))) stop 123
      end do
    !$OMP END PARALLEL

    !$OMP PARALLEL firstprivate(val1)  ! { dg-error "Sorry, polymorphic arrays not yet supported for firstprivate" }
      select type (val1)
        type is (integer)
          if (size(val1) /= 4) stop 33
          if (any (val1 /= [1, 2, 3, 4])) stop 4549
          val1 = [32,6,48,28]
        class default
          stop 99
      end select
      select type (val1)
        type is (integer)
          if (size(val1) /= 4) stop 33
          if (any (val1 /= [32,6,48,28])) stop 4512
        class default
          stop 99
      end select
    !$OMP END PARALLEL

    select type (val1)
      type is (integer)
        if (size(val1) /= 4) stop 33
        if (any (val1 /= [1, 2, 3, 4])) stop 454
      class default
        stop 99
    end select
    print *, "PASS!"
  end subroutine
end program select_type_openmp
