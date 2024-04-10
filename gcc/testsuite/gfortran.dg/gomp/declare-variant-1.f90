module main
  implicit none

  interface
    integer function foo (a, b, c)
      integer, intent(in) :: a, b
      integer, intent(inout) :: c
    end function

    integer function bar (a, b, c)
      integer, intent(in) :: a, b
      integer, intent(inout) :: c
    end function

    integer function baz (a, b, c)
      integer, intent(in) :: a, b
      integer, intent(inout) :: c

      !$omp declare variant (foo) &
      !$omp & match (construct={parallel,do}, &
      !$omp & device={isa(avx512f,avx512vl),kind(host,cpu)}, &
      !$omp & implementation={vendor(score(0):gnu),unified_shared_memory}, &
      !$omp & user={condition(score(0):.false.)})
      !$omp declare variant (bar) &
      !$omp & match (device={arch(x86_64,powerpc64),isa(avx512f,popcntb)}, &
      !$omp & implementation={atomic_default_mem_order(seq_cst),made_up_selector("foo", 13, "bar")}, &
      !$omp & user={condition(.true. .AND. (.not. .true.))})
! { dg-warning "unknown selector 'made_up_selector'" "" { target *-*-* } .-2 }
    end function

    subroutine quux
    end subroutine quux

    integer function baz3 (x, y, z)
      integer, intent(in) :: x, y
      integer, intent(inout) :: z

      !$omp declare variant (bar) match &
      !$omp & (implementation={atomic_default_mem_order(score(3): acq_rel)})
    end function
  end interface
contains
  integer function qux ()
    integer :: i = 3

    qux = baz (1, 2, i)
  end function

  subroutine corge
    integer :: i
    !$omp declare variant (quux) match (construct={parallel,do})

    interface
      subroutine waldo (x)
        integer, intent(in) :: x
      end subroutine
    end interface

    call waldo (5)
    !$omp parallel do
      do i = 1, 3
	call waldo (6)
      end do
    !$omp end parallel do

    !$omp parallel
      !$omp taskgroup
	!$omp do
	  do i = 1, 3
	    call waldo (7)
	  end do
        !$omp end do
      !$omp end taskgroup
    !$omp end parallel

    !$omp parallel
      !$omp master
        call waldo (8)
      !$omp end master
    !$omp end parallel
  end subroutine

  integer function baz2 (x, y, z)
    integer, intent(in) :: x, y
    integer, intent(inout) :: z

    !$omp declare variant (bar) match &
    !$omp & (implementation={atomic_default_mem_order(relaxed), &
    !$omp &		   unified_address, unified_shared_memory, &
    !$omp &		   dynamic_allocators, reverse_offload})

    baz2 = x + y + z
  end function
end module
