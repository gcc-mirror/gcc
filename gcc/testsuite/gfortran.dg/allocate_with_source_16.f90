!{ dg-do compile }
! PR69268
!
! Contributed by Rich Townsend  <townsend@astro.wisc.edu>

program test_sourced_alloc

  implicit none
 
  type :: foo_t
  end type foo_t

  class(foo_t), allocatable :: f

  allocate(f, SOURCE=f_func())

contains

  function f_func () result (f)
    type(foo_t) :: f
    integer, save :: c = 0
    c = c + 1
    if (c .gt. 1) call abort()
  end function f_func

end program test_sourced_alloc 
