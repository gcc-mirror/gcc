! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 60922: [4.9/5 regression] Memory leak with allocatable CLASS components
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

program test_leak
  implicit none

  type d_base_vect_type
  end type

  type d_vect_type
    class(d_base_vect_type), allocatable :: v
  end type

  call test()

contains

  subroutine test()
    class(d_vect_type), allocatable :: x
    allocate(x)
    allocate(x%v)
    print *,"allocated!"
  end subroutine

end

! { dg-final { scan-tree-dump-times "fini_coarray" 1 "original" } }
