! { dg-do run }
! PR 21354:  Rank 7 was not handled correctly by many library
!            functions, including in_pack.
program main
  real, dimension (2,2,2,2,2,2,2):: a
  a = 1.0
  call foo(a(2:1:-1,:,:,:,:,:,:))
end program main

subroutine foo(a)
  real, dimension (2,2,2,2,2,2,2):: a
end subroutine foo
