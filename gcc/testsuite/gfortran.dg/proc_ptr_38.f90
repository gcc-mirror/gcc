! { dg-do compile }
!
! PR 54387: [F03] Wrongly accepts non-proc result variable on the RHS of a proc-pointer assignment
!
! Contributed by James Van Buskirk

integer function foo()
  procedure(), pointer :: i
  i => foo  ! { dg-error "is invalid as proc-target in procedure pointer assignment" }
end 

recursive function bar() result (res)
  integer :: res
  procedure(), pointer :: j
  j => bar
end
