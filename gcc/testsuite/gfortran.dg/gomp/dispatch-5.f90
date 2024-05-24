! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

module main
  implicit none
    interface
      subroutine f2 (a)
        integer, intent(in) :: a
      end subroutine
    end interface
  contains
  
  subroutine test ()
    integer :: a

  !$omp dispatch device(-25373654)
    ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(-25373654\\);" 1 "gimple" } } 
    call f2 (a)
  !$omp dispatch device(a + a)
    ! { dg-final { scan-tree-dump-times "(D\.\[0-9]+) = a.\[0-9_]+ \\* 2;.*#pragma omp dispatch.*__builtin_omp_set_default_device \\(\\1\\);.*f2 \\(&a\\)" 2 "gimple" } }
    call f2 (a)
  end subroutine
end module

! { dg-final { scan-tree-dump-times "(D\.\[0-9]+) = __builtin_omp_get_default_device \\(\\);.*__builtin_omp_set_default_device \\(\\1\\);" 4 "gimple" } }
