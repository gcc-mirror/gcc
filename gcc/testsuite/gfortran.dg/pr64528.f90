! PR fortran/64528
! { dg-do compile }
! { dg-options "-O -fno-tree-dce -fno-tree-ccp" }

program pr64528
  interface
     subroutine foo(x)
       integer, value :: x
     end subroutine foo
  end interface
  integer :: x
  x = 10
  call foo(x)
  if(x .ne. 10) then
  endif
end program pr64528
subroutine foo(x)
  integer, value :: x
  x = 11
end subroutine foo
