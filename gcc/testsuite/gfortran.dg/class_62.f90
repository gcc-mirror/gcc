! { dg-do run }
! { dg-options "-fcheck=recursion" }
!
! PR 80361: [5/6/7 Regression] bogus recursive call to nonrecursive procedure with -fcheck=recursion
!
! Contributed by Jürgen Reuter <juergen.reuter@desy.de>

program main_ut

  implicit none

  type :: prt_spec_expr_t
  end type

  type :: prt_expr_t
     class(prt_spec_expr_t), allocatable :: x
  end type

  type, extends (prt_spec_expr_t) :: prt_spec_list_t
     type(prt_expr_t) :: e
  end type

  class(prt_spec_list_t), allocatable :: y

  allocate (y)
  allocate (prt_spec_list_t :: y%e%x)
  deallocate(y)

end program
