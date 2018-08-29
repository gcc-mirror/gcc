! { dg-do run }
! { dg-options "-finit-character=89" }
!
! PR fortran/51800
!

subroutine foo(n)
  character(len=n) :: str
!  print *, str
  if (str /= repeat ('Y', n)) STOP 1
end subroutine foo

call foo(3)
call foo(10)
end
