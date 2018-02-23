! { dg-do run }
! PR 20361 : We didn't check if a large equivalence actually fit on
! the stack, and therefore segfaulted at execution time
subroutine test
integer i(1000000), j
equivalence (i(50), j)

j = 1
if (i(50) /= j) STOP 1
end subroutine test

call test
end
