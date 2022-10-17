! { dg-do compile }

! The base procedure must have an accessible explicit interface when the
! directive appears.

program main
  interface
    subroutine base_proc ()
    end subroutine
  end interface

  !$omp declare variant (base_proc: variant_proc) match (construct={parallel})
  !$omp declare variant (base_proc2: variant_proc) match (construct={parallel}) ! { dg-error "The base procedure at .1. must have an explicit interface" }
contains
  subroutine variant_proc ()
  end subroutine
end program
