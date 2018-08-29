! { dg-do compile }
! { dg-options "-finit-derived" }
!
! PR fortran/83183
!
! Test a regression where -finit-derived recursed infinitely generating
! initializers for allocatable components of the same derived type.
!

program pr83183
  type :: linked_list
     type(linked_list), allocatable :: link
     integer :: value
  end type
  type(linked_list) :: test
  allocate(test % link)
  print *, test%value
  print *, test%link%value
end program
