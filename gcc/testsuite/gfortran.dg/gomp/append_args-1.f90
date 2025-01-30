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
  subroutine f1a ()
  end

  subroutine f1b ()
  end

  subroutine f1c ()
  end

  subroutine f1d ()
  end

  subroutine f1e ()
  end

  subroutine f1po (q,r, obj)
    type(c_ptr) :: q, r
    value :: r
    integer(omp_interop_kind),value :: obj
  end

  subroutine f2 ()
    !$omp declare variant (f1a) match(user={condition(flag)}) &
    !$omp&     append_args ( interop ( target , targetsync) ) match(construct={dispatch})   ! { dg-error "'match' clause at .1. specified more than once" }
  end subroutine

  subroutine f2a ()
    !$omp declare variant (f1b) append_args ( interop ( prefer_type ( "cuda", "hip" ) ) , interop(target)) &
    !$omp&     append_args ( interop ( target , targetsync) ) match(construct={dispatch})   ! { dg-error "'append_args' clause at .1. specified more than once" }
  end subroutine


  subroutine f2b ()
    !$omp declare variant (f1c)  &
    !$omp&     append_args ( interop ( target , targetsync) )   ! { dg-error "the 'append_args' clause at .1. can only be specified if the 'dispatch' selector of the construct selector set appears in the 'match' clause" } 
  end subroutine

  subroutine f2c (x,y)
    !$omp declare variant (fop) , append_args ( interop ( prefer_type ( "cuda", "hip" ) ) , interop(target)) , &
    !$omp&     adjust_args (need_device_ptr : x, y )   ! { dg-error "the 'adjust_args' clause at .1. can only be specified if the 'dispatch' selector of the construct selector set appears in the 'match' clause" } 
    type(c_ptr) :: x, y
    value :: y
  end subroutine

  subroutine f2d ()
    !$omp declare variant (f1d) append_args ( interop ( prefer_type ( "cuda", "hip" ) ) , interop(target)) ,  ! { dg-error "111: expected 'match', 'adjust_args' or 'append_args' at .1." }
  end subroutine

  subroutine f2e ()
    !$omp declare variant (f1e) append_args ( interop ( prefer_type ( "cuda", "hip" ) ) , interop(target) interop(targetsync))  ! { dg-error "Expected ',' or '\\)' at .1." }
  end subroutine
end
