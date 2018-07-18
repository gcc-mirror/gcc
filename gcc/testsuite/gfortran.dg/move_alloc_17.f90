! { dg-do compile }
!
! The call to MOVE_ALLOC below caused a seg fault in runtime.
! This was discussed in:
! https://groups.google.com/forum/#!topic/comp.lang.fortran/ZVLqXFYDZ0M
! Richard Maine proposed that the code violated the restrictions on
! actual arguments in F2003 12.4.1.7 and so the fix asserts that the
! TO and FROM arguments cannot be the same object or subobjects thereof.
!
!
program test_move_alloc
  type :: linked_list
     type(linked_list), allocatable :: link
     integer :: value
  end type linked_list
  type(linked_list) :: test

  allocate(test % link)
  allocate(test % link % link)
  call move_alloc(test % link, test % link % link) ! { dg-error "aliasing restrictions" }
end program test_move_alloc
