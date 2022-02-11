! { dg-do compile }
! { dg-options "-fcoarray=single" }
! PR 103054 - wrong keyword name.
! Original test case by Damian Rouson.
program main
  implicit none
  logical :: co_all= .true.
  call co_reduce(co_all, operator=both) ! { dg-error "Cannot find keyword" }
  call co_reduce(co_all, operation=both)
contains
  logical pure function both(lhs,rhs)
    logical, intent(in) :: lhs, rhs
    both = lhs .and. rhs
  end function
end
