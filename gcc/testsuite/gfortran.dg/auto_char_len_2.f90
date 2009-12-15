! { dg-do compile }
! { dg-options "-fwhole-file" }
!
! PR fortran/41235
!

character(len=*) function func()
  func = 'ABC'
end function func

subroutine test(i)
  integer :: i
  character(len=i), external :: func
  print *, func()
end subroutine test

subroutine test2(i)
  integer :: i
  character(len=i) :: func
  print *, func()
end subroutine test2

call test(2)
call test2(2)
end
