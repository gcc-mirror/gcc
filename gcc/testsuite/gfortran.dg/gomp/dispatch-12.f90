! The following definitions are in omp_lib, which cannot be included
! in gcc/testsuite/

module m
  use iso_c_binding
  implicit none (type, external)

  integer, parameter :: omp_interop_kind = c_intptr_t
  integer, parameter :: omp_interop_none = 0_omp_interop_kind

  interface
    subroutine repl1(); end

    subroutine base1()
      !$omp declare variant(repl1) match(construct={dispatch})
    end
  end interface

contains
  subroutine test (obj1)
    integer(omp_interop_kind), intent(in) :: obj1
    integer(omp_interop_kind) :: obj2(2)
    integer(omp_interop_kind), parameter :: obj3 = omp_interop_none
    integer(1) :: x

    !$omp dispatch interop ( obj1, obj2, obj1 ) device(2) ! { dg-error "'obj2' at .1. in 'INTEROP' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
      call base1 ()

    !$omp dispatch interop ( obj1, obj1, obj1 ) device(2) ! OK
      call base1 ()

    !$omp dispatch interop ( obj3 ) ! { dg-error "Object 'obj3' is not a variable at .1." }
      call base1 ()
      ! { dg-error "'obj3' at .1. in 'INTEROP' clause must be a scalar integer variable of 'omp_interop_kind' kind" "" { target *-*-* } .-2 }

    !$omp dispatch interop ( obj1 )
      call base1 ()

    !$omp dispatch interop ( obj2 )  ! { dg-error "'obj2' at .1. in 'INTEROP' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
      call base1 ()

    !$omp dispatch interop ( x )  ! { dg-error "'x' at .1. in 'INTEROP' clause must be a scalar integer variable of 'omp_interop_kind' kind" }
      call base1 ()

    !$omp dispatch interop ( obj1) device(2) interop (obj1 ) ! { dg-error "Duplicated 'interop' clause" }
      call base1 ()

  end
end module
