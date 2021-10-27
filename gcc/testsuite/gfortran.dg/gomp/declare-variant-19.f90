! { dg-do compile }

! Test Fortran-specific compilation failures.

module main
  implicit none
  
  interface base_gen
    subroutine base_gen_int (x)
      integer :: x
    end subroutine

    subroutine base_gen_real (x)
      real :: x
    end subroutine
  end interface

  interface
    subroutine base_p ()
    end subroutine
  end interface

  procedure (base_p), pointer :: base_proc_ptr

  !$omp declare variant (base_entry: variant) match (construct={parallel}) ! { dg-error "The base name at .1. must not be an entry name" }
  !$omp declare variant (base_proc_ptr: variant) match (construct={parallel}) ! { dg-error "The base name at .1. must not be a procedure pointer" }
  !$omp declare variant (base_gen: variant2) match (construct={parallel}) ! { dg-error "The base name at .1. must not be a generic name" }
  !$omp declare variant (variant) match (construct={parallel}) ! { dg-error "The base name for 'declare variant' must be specified at .1." }
  
contains
  subroutine base ()
    entry base_entry
  end subroutine

  subroutine base2 ()
    !$omp declare variant (variant2) match (construct={parallel})   ! { dg-error "variant .variant2. and base .base2. at .1. have incompatible types: .variant2. has the wrong number of arguments" }
  end subroutine

  subroutine base3 ()
    !$omp declare variant (base: variant2) match (construct={parallel}) ! { dg-error "The base name at .1. does not match the name of the current procedure" }
  end subroutine

  subroutine variant ()
  end subroutine

  subroutine variant2 (x)
    integer :: x
  end subroutine
end module
