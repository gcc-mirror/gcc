! { dg-do link }
! { dg-additional-options "-fdump-tree-gimple" }
! { dg-additional-sources "declare-variant-mod-2-use.f90" }

! Note: We have to use 'link' as otherwise '-o' is specified,
! which does not work with multiple files.

! Error message in the additional-sources file:

! { dg-error "'x' at .1. is specified more than once" "" { target *-*-* } 17 }


! Check that module-file handling works for declare_variant
! and its match/adjust_args/append_args clauses
!
! PR fortran/115271

module m1
  implicit none (type, external)
contains
  integer function m1_f (x, y, z)
    use iso_c_binding
    type(c_ptr) :: x, y, z
    value :: x
    m1_f = 1
  end
  integer function m1_g (x, y, z)
    use iso_c_binding
    type(c_ptr) :: x, y, z
    value :: x
    m1_g = 2
  end
end module m1

module m2
  use iso_c_binding, only: c_intptr_t
  implicit none (type, external)
  integer, parameter :: omp_interop_kind = c_intptr_t

  !$omp declare variant(m2_g : m2_f3) match(construct={do,dispatch}, device={kind(host)}) &
  !$omp&   append_args(interop(target),interop(targetsync), interop(prefer_type({fr("cuda"), attr("ompx_A")}, {fr("hip")}, {attr("ompx_B")}), targetsync))

contains
  subroutine m2_f3 (x, obj1, obj2, obj3)
    use iso_c_binding
    integer(omp_interop_kind) :: obj1, obj2, obj3
    value :: obj1
    integer, value :: x
  end

  subroutine m2_f2 (x, obj1, obj2)
    use iso_c_binding
    integer(omp_interop_kind) :: obj1, obj2
    integer, value :: x
  end

  subroutine m2_f1 (x, obj1)
    use iso_c_binding
    integer(omp_interop_kind), value :: obj1
    integer, value :: x
  end

  subroutine m2_g (x)
    integer, value :: x
    !$omp declare variant(m2_g : m2_f1) match(construct={dispatch}) append_args(interop(target, targetsync, prefer_type("cuda", "hip")))
    !$omp declare variant(m2_f2) match(construct={parallel,dispatch}, implementation={vendor("gnu")}) append_args(interop(target),interop(targetsync))
  end
end module
