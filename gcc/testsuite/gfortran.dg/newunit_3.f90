! { dg-do run }
! PR48960 On ERROR newunit should not modify user variable.
program test_newunit
    integer :: st, un = 0
    open (newunit=un, file='nonexisting.dat', status='old', iostat=st)
    if (un /= 0) call abort
end program test_newunit
