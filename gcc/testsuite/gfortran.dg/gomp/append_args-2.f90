! { dg-do compile }
! { dg-additional-options "-fcoarray=single" }

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
  subroutine f1o (obj)
    integer(omp_interop_kind),value :: obj
  end

  subroutine f1ox (q,r, obj)
  ! { dg-error "'q' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    type(c_ptr) :: q, r
    value :: r
    integer(omp_interop_kind),value :: obj
  end


  subroutine f5 ()
    !$omp declare variant (f1ox) match(user={condition(flag)}) & ! { dg-error "the 'append_args' clause can only be specified if the 'dispatch' selector of the construct selector set appears in the 'match' clause at .1." }
    !$omp&  append_args ( interop ( target , targetsync) )
    ! { dg-error "'q' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
  end subroutine

  subroutine f6 (x, y)
    !$omp declare variant (f1ox) match(user={condition(flag)}) & ! { dg-error "the 'adjust_args' clause can only be specified if the 'dispatch' selector of the construct selector set appears in the 'match' clause at .1." }
    !$omp&  append_args ( interop ( target , targetsync) ) &
    !$omp&  adjust_args ( need_device_ptr : x , y)
    type(c_ptr) :: x, y
    value :: y
  end subroutine

  subroutine g1 (obj, obj2, obj3)
    integer(omp_interop_kind),value :: obj,obj3
    integer(omp_interop_kind),value :: obj2
  end
  subroutine g1a (obj)
    !$omp declare variant (g1 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    integer(omp_interop_kind),value :: obj
  end

  subroutine g2 (obj, obj2, obj3)
    ! { dg-error "'g2' at .1. has 2 but requires 1 'omp_interop_kind' kind dummy arguments as it is utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj,obj2,obj3
  end
  subroutine g2a (obj)
    !$omp declare variant (g2 ) match(construct={dispatch}) append_args ( interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")}), targetsync))
    ! { dg-error "'g2' at .1. has 2 but requires 1 'omp_interop_kind' kind dummy arguments as it is utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g3 (obj, obj2, obj3)
    integer(omp_interop_kind),value :: obj,obj3
    integer(omp_interop_kind) :: obj2
  end
  subroutine g3a (obj)
    !$omp declare variant (g3 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    integer(omp_interop_kind),value :: obj
  end

  subroutine g4 (obj, obj2, obj3)
    integer(omp_interop_kind),value :: obj,obj3
    integer(omp_interop_kind) :: obj2
  end
  subroutine g4a (obj)
    !$omp declare variant (g4 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    integer(omp_interop_kind),value :: obj
  end

  subroutine g5 (obj, obj2, obj3)
    ! { dg-error "'obj3' at .1. with OPTIONAL attribute not support when utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj,obj3
    integer(omp_interop_kind) :: obj2
    optional :: obj3
  end
  subroutine g5a (obj)
    !$omp declare variant (g5 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj3' at .1. with OPTIONAL attribute not support when utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g5var (obj, obj2, obj3)
    ! { dg-error "'obj3' at .1. with OPTIONAL attribute not support when utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind) :: obj,obj3
    integer(omp_interop_kind) :: obj2
    value :: obj
    optional :: obj3
  end
  subroutine g5avar (obj)
    !$omp declare variant (g5var ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj3' at .1. with OPTIONAL attribute not support when utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g6 (obj, obj2, obj3)
    ! { dg-error "'obj3' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
    integer(omp_interop_kind),pointer :: obj3
    integer(omp_interop_kind) :: obj2
  end
  subroutine g6a (obj)
    !$omp declare variant (g6 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj3' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g7 (obj, obj2, obj3)
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
    integer(omp_interop_kind) :: obj3
    integer(omp_interop_kind),allocatable :: obj2
  end
  subroutine g7a (obj)
    !$omp declare variant (g7 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g8 (obj, obj2, obj3)
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
    integer(omp_interop_kind) :: obj3
    integer(omp_interop_kind) :: obj2(:)
  end
  subroutine g8a (obj)
    !$omp declare variant (g8 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g9 (obj, obj2, obj3)
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
    integer(omp_interop_kind) :: obj3
    integer(omp_interop_kind) :: obj2(2)
  end
  subroutine g9a (obj)
    !$omp declare variant (g9 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g10 (obj, obj2, obj3)
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
    integer(omp_interop_kind) :: obj3
    integer(1) :: obj2
  end
  subroutine g10a (obj)
    !$omp declare variant (g10 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g11 (obj, obj2, obj3)
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
    integer(omp_interop_kind) :: obj3
    real(omp_interop_kind) :: obj2  ! { dg-warning "C kind type parameter is for type INTEGER but type at .1. is REAL" }
  end
  subroutine g11a (obj)
    !$omp declare variant (g11 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end

  subroutine g12 (obj, obj2, obj3)
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
    integer(omp_interop_kind) :: obj3
    integer(omp_interop_kind) :: obj2[*]
  end
  subroutine g12a (obj)
    !$omp declare variant (g12 ) match(construct={dispatch}) append_args ( interop ( target , targetsync), interop( target, prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ! { dg-error "'obj2' at .1. must be a nonpointer, nonallocatable scalar integer dummy argument of 'omp_interop_kind' kind as it utilized with the 'append_args' clause at .2." "" { target *-*-* } .-1 }
    integer(omp_interop_kind),value :: obj
  end
end
