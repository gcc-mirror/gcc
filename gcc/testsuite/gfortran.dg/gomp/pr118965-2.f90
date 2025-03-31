! At least one of the target and/or targetsync modifiers must be provided.

module my_omp_lib
 use iso_c_binding
 implicit none

 ! The following definitions are in omp_lib, which cannot be included
 ! in gcc/testsuite/
 integer, parameter :: omp_interop_kind = c_intptr_t
 integer, parameter :: omp_interop_fr_kind = c_int

 integer (omp_interop_kind), parameter :: omp_interop_none = 0_omp_interop_kind
 integer (omp_interop_fr_kind), parameter :: omp_ifr_cuda = 1
 integer (omp_interop_fr_kind), parameter :: omp_ifr_cuda_driver = 2
 integer (omp_interop_fr_kind), parameter :: omp_ifr_opencl = 3
 integer (omp_interop_fr_kind), parameter :: omp_ifr_sycl = 4
 integer (omp_interop_fr_kind), parameter :: omp_ifr_hip = 5
 integer (omp_interop_fr_kind), parameter :: omp_ifr_level_zero = 6
 integer (omp_interop_fr_kind), parameter :: omp_ifr_hsa = 7
end module my_omp_lib

module m
  use my_omp_lib
  implicit none
  logical, parameter :: flag = .true.
contains

  subroutine f1 (i)
    integer(omp_interop_kind) :: i
  end

  subroutine g1 ()
    !$omp declare variant (f1) match(user={condition(flag)}) &
    !$omp&    append_args(interop(prefer_type({attr("ompx_fun")})))
    ! { dg-error "Missing required 'target' and/or 'targetsync' modifier" ""  { target *-*-* } .-1 }
  end

  function f2 (a1, a2)
    integer(omp_interop_kind) :: a1
    integer(omp_interop_kind) :: a2
    integer :: f2
    f2 = 0
  end

  function g2 ()
    !$omp declare variant(f2) &
    !$omp&    append_args(interop(prefer_type("cuda")), &
    !$omp&    interop(prefer_type({fr("hsa")}))) &
    !$omp&    match(construct={dispatch})
    ! { dg-error "Missing required 'target' and/or 'targetsync' modifier" ""  { target *-*-* } .-3 }
    ! There is no diagnostic for the second interop arg because Fortran
    ! error recovery skips to the end of the statement after diagnosing the
    ! first one.
    integer :: g2
    g2 = 5
  end
end
