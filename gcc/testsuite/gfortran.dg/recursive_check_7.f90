! { dg-do run }
! { dg-options "-fcheck=recursion" }
! { dg-shouldfail "Recursion check" }
!
! PR fortran/32626
! Recursion run-time check
!

subroutine NormalFunc()
end subroutine NormalFunc

recursive subroutine valid(x)
  logical :: x
  if(x) call sndValid()
  print *, 'OK'
end subroutine valid

subroutine sndValid()
  call valid(.false.)
end subroutine sndValid

subroutine invalid(x)
  logical :: x
  if(x) call sndInvalid()
  print *, 'BUG'
  STOP 1
end subroutine invalid

subroutine sndInvalid()
  call invalid(.false.)
end subroutine sndInvalid

call valid(.true.)
call valid(.true.)
call NormalFunc()
call NormalFunc()
call invalid(.true.)
end

! { dg-output "Fortran runtime error: Recursive call to nonrecursive procedure 'invalid'" }
