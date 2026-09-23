! Check for comma between directive and first clause
! Additionally, check that no space is requires between directive(...)
! and the first clause

module my_omp_lib_kinds
        integer, parameter :: omp_lock_hint_kind = 4
        integer (omp_lock_hint_kind), &
                 parameter :: omp_sync_hint_none = 0
        integer (omp_lock_hint_kind), &
                 parameter :: omp_sync_hint_uncontended = 1
        integer (omp_lock_hint_kind), &
                 parameter :: omp_sync_hint_contended = 2
        integer (omp_lock_hint_kind), &
                 parameter :: omp_sync_hint_nonspeculative = 4
        integer (omp_lock_hint_kind), &
                 parameter :: omp_sync_hint_speculative = 8
end module 

module m
  use iso_c_binding, only: c_size_t
  use my_omp_lib_kinds
  implicit none

  ! from omp_lib:
  integer, parameter :: omp_depend_kind = 2*c_size_t

  !$omp requires,dynamic_allocators
  !$omp requires , reverse_offload

  !$omp assumes,contains(parallel)
  !$omp assumes , holds(1 > 0)
  type t
    integer :: x
  end type
  !$omp declare mapper(t :: v)map(v%x)
  !$omp declare mapper(other : t :: v),map(v%x)
  !$omp declare mapper(other2 : t :: v) , map(v%x)
  !$omp declare_mapper(other3 : t :: v)map(v%x)
  !$omp declare_mapper(other4 : t :: v),map(v%x)
  !$omp declare_mapper(other5 : t :: v) , map(v%x)

  !$omp declare reduction(one1 : t : omp_out%x = omp_out%x + omp_in%x)initializer(omp_priv%x = 0)
  !$omp declare reduction(one2 : t : omp_out%x = omp_out%x + omp_in%x),initializer(omp_priv%x = 0)
  !$omp declare reduction(one3 : t : omp_out%x = omp_out%x + omp_in%x) , initializer(omp_priv%x = 0)
  !$omp declare_reduction(one4 : t : omp_out%x = omp_out%x + omp_in%x)initializer(omp_priv%x = 0)
  !$omp declare_reduction(one5 : t : omp_out%x = omp_out%x + omp_in%x),initializer(omp_priv%x = 0)
  !$omp declare_reduction(one6 : t : omp_out%x = omp_out%x + omp_in%x) , initializer(omp_priv%x = 0)

interface
    integer function bar1 (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : val, step (1))linear (y : step (2))
    end
    integer function bar2 (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : val, step (1)),linear (y : step (2))
    end
    integer function bar3 (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : val, step (1)) , linear (y : step (2))
    end
    integer function bar4 (x, y, z)
      integer, value :: x, y, z
      !$omp declare_simd linear (x : val, step (1))linear (y : step (2))
    end
    integer function bar5 (x, y, z)
      integer, value :: x, y, z
      !$omp declare_simd linear (x : val, step (1)),linear (y : step (2))
    end
    integer function bar6 (x, y, z)
      integer, value :: x, y, z
      !$omp declare_simd linear (x : val, step (1)) , linear (y : step (2))
    end
end interface

  integer :: a1,a2,a3,a4
  !$omp declare target,enter(a1)
  !$omp declare target , enter (a2)
  !$omp declare_target,enter(a3)
  !$omp declare_target , enter( a4 )

contains
  subroutine sub1; end subroutine
  subroutine sub2; end subroutine
  subroutine sub3; end subroutine
  subroutine sub4; end subroutine
  subroutine sub5; end subroutine
  subroutine sub6; end subroutine
  subroutine sub
    !$omp declare variant(sub1)match(construct={parallel})
    !$omp declare variant(sub2),match(construct={target})
    !$omp declare variant(sub3) , match(construct={teams})
    !$omp declare_variant(sub4)match(construct={target,teams})
    !$omp declare_variant(sub5),match(user={condition(huge(a2) > 0)})
    !$omp declare_variant(sub6) , match(user={condition(kind(a1) < 0)})

    !$omp assume,holds(a1 > 0)
      !$omp assume , holds(a2 < 0)
      !$omp end assume
    !$omp end assume

    !$omp begin metadirective,when(user={condition(a3 > 0)}: parallel)
      a1 = 0
    !$omp end metadirective
    !$omp begin metadirective , when(user={condition(a4 > 0)}: parallel)
      a2 = 0
    !$omp end metadirective

    !$omp error,at(execution),message("Hello")
    !$omp error , at(execution) , message("world")

    !$omp metadirective,when(user={condition(a1 /= a2)}: flush)
    !$omp metadirective , when(user={condition(a2 /= a3)}: taskwait)
  end subroutine

  subroutine othersub
    integer :: i, j

    !$omp simd,private(j)
      do i = 1,1; j = i; end do
    !$omp simd , private(j)
      do i = 1,1; j = i; end do

    !$omp do,private(j)
      do i = 1,1; j = i; end do
    !$omp do, private(j)
      do i = 1,1; j = i; end do

    !$omp do simd,private(j)
      do i = 1,1; j = i; end do
    !$omp do simd , private(j)
      do i = 1,1; j = i; end do

    !$omp target parallel,firstprivate(j)
      !$omp cancellation point,parallel

      !$omp critical,hint(omp_sync_hint_none)
      !$omp end critical

      !$omp critical , hint(omp_sync_hint_none)
      !$omp end critical

      !$omp critical(ABC1),hint(omp_sync_hint_none)
      !$omp end critical(ABC1)

      !$omp critical ( ABC2 ),hint(omp_sync_hint_none)
      !$omp end critical ( ABC2 )

      !$omp critical (ABC3) , hint(omp_sync_hint_none)
      !$omp end critical (ABC3)

      !$omp cancel,parallel
    !$omp end target parallel,nowait 

    !$omp target parallel , firstprivate(j)
      !$omp cancellation point , parallel
      !$omp cancel , parallel
    !$omp end target parallel , nowait 

    !$omp parallel , firstprivate(j)
      !$omp cancellation_point , parallel
      !$omp cancel,parallel
    !$omp end parallel

    !$omp parallel , firstprivate(j)
      !$omp cancellation point , parallel
      !$omp cancel , parallel
    !$omp end parallel

    !$omp atomic,update
      a1 = a1 + 1
    !$omp end atomic
    !$omp atomic , read
      j = a1
    !$omp end atomic

    !$omp single,nowait
    !$omp end single
    !$omp single , nowait
    !$omp end single
    !$omp single

    !$omp end single,nowait
    !$omp single
    !$omp end single , nowait
  end

  subroutine foo
    integer, save :: i, j, k
    !$omp groupprivate(k)device_type(nohost)
    !$omp groupprivate(i),device_type(nohost)
    !$omp groupprivate(j) , device_type(nohost)
  end

  subroutine bar
    integer(omp_depend_kind) :: obj
    !$omp depobj(obj),depend(in: a1)
    !$omp depobj(obj)destroy
    !$omp depobj(obj) , destroy
  end
end module m
