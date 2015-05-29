! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 49466: [4.6/4.7 Regression] Memory leak with assignment of extended derived types
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

program evolve_aflow

  implicit none

  type :: state_t
     real, allocatable :: U(:)
  end type

  type, extends(state_t) :: astate_t
  end type

 block ! New scoping unit as "a"/"b" are otherwise implicitly SAVEd
  type(astate_t) :: a,b

  allocate(a%U(1000))

  a = b
 end block
end program 

! { dg-final { scan-tree-dump-times "__builtin_free" 3 "original" } }
