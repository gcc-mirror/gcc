! { dg-do run }
! { dg-options "-Warray-temporaries" }
! PR 56867 - substrings were not checked for dependency.
program main
  character(len=4) :: a
  character(len=4) :: c(3)
  c(1) = 'abcd'
  c(2) = '1234'
  c(3) = 'wxyz'
  c(:)(1:2) = c(2)(2:3)   ! { dg-warning "array temporary" }
  if (c(3) .ne. '23yz') call abort
end program main
