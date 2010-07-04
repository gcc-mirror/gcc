! { dg-do "compile" }
!
! Verify that the outer do-loop counter 'j' is accepted as
! as end-expression of the inner loop.
!

  integer i, j
  integer, parameter :: n = size( [( [(i*j,i=1,j)], j=1,2)] )
end
