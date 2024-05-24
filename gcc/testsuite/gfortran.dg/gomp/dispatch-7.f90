! { dg-do compile }
! { dg-additional-options "-fdump-tree-ompexp" }

module main
  use iso_c_binding, only: c_ptr
  implicit none
  interface
  subroutine f2 (p)
    import :: c_ptr
    type(c_ptr), intent(out) :: p
  end subroutine
  end interface
  contains
  
  subroutine test ()
    type(c_ptr) :: p

  !$omp dispatch
    ! { dg-final { scan-tree-dump-not "__builtin_GOMP_task " "ompexp" } }
    call f2 (p)
  !$omp dispatch depend(inout: p)
    ! { dg-final { scan-tree-dump-times "(D\.\[0-9]+)\\\[2] = &p;\[ \n]*__builtin_GOMP_taskwait_depend \\(&\\1\\);" 2 "ompexp" } }
    call f2 (p)
  end subroutine
end module

