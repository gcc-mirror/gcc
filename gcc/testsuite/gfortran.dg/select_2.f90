! { dg-do run }
!  Simple test program to see if gfortran eliminates the 'case (3:2)'
!  statement.  This is an unreachable CASE because the range is empty.
!
program select_3
  integer i
  do i = 1, 4
     select case(i)
     case (1)
       if (i /= 1) call abort
     case (3:2)
       call abort
     case (4)
       if (i /= 4) call abort
     case default
       if (i /= 2 .and. i /= 3) call abort
     end select
  end do
end program select_3
