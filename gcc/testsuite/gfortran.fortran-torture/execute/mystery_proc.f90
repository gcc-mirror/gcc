! Program to test dummy procedures
subroutine bar()
end subroutine

subroutine foo2(p)
  external p

  call p()
end subroutine

subroutine foo(p)
  external p
  ! We never actually discover if this is a function or a subroutine
  call foo2(p)
end subroutine

program intrinsic_minmax
   implicit none
   external bar

   call foo(bar)
end program

