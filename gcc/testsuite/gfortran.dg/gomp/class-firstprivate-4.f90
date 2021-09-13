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
  integer x(4)
  x = [1, 2, 3, 4]
  call sub(x)
  if (any (x /= [1,2,3,4])) stop 3
contains
  subroutine sub(val1)
    integer :: i
    class(*) :: val1(4)
 
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
          if (size(val1) /= 4) stop 34
          if (any (val1 /= [32,6,48,28])) stop 4512
        class default
          stop 98
      end select
    !$OMP END PARALLEL
  end
end
