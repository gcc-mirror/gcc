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
  !use omp_lib
  implicit none
  class(*), allocatable :: B(:)

 allocate(B, source=["abcdef","cdefi2"])
 allocate(B, source=[1,2,3])
 call sub(B)
contains
  subroutine sub(val2)
    class(*), allocatable :: val2(:)

    !$OMP PARALLEL firstprivate(val2)  ! { dg-error "Sorry, polymorphic arrays not yet supported for firstprivate" }
      if (.not.allocated(val2)) stop 3
      select type (val2)
        type is (character(len=*))
          if (len(val2) /= 6) stop 44
          if (val2(1) /= "abcdef" .or. val2(2) /= "cdefi2") stop 4545
          val2 = ["123456", "789ABC"]
        class default
          stop 991
      end select
      select type (val2)
        type is (character(len=*))
          if (len(val2) /= 6) stop 44
          if (val2(1) /= "123456" .or. val2(2) /= "789ABC") stop 453
        class default
          stop 991
      end select
    !$OMP END PARALLEL

    if (.not.allocated(val2)) stop 3
    select type (val2)
      type is (character(len=*))
        if (len(val2) /= 6) stop 44
        if (val2(1) /= "abcdef" .or. val2(2) /= "cdefi2") stop 456
      class default
        stop 991
    end select
    print *, "PASS!"
  end subroutine
end program select_type_openmp
