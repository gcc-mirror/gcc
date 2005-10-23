! { dg-do run }
! { dg-options "-std=gnu" }
program testbyte
  integer(1) :: ii = 7
  call foo(ii)
end program testbyte

subroutine foo(ii)
  integer(1) ii
  byte b
  b = ii
  call bar(ii,b)
end subroutine foo

subroutine bar(ii,b)
  integer (1) ii
  byte b
  if (b.ne.ii) then
!     print *,"Failed"
     call abort
  end if
end subroutine bar
