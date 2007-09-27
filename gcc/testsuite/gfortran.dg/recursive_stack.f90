! { dg-do run }
! { dg-options "-frecursive" }
program recursive_stack
  call foo (.true.)
end program recursive_stack

subroutine foo (recurse)
  logical recurse
  integer iarray(100,100)
  if (recurse) then
     iarray(49,49) = 17
     call bar
     if (iarray(49,49) .ne. 17) call abort
  else
     iarray(49,49) = 21
  end if
end subroutine foo

subroutine bar
  call foo (.false.)
end subroutine bar
