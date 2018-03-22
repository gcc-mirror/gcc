! { dg-do run }
!  Simple test program to see if gfortran eliminates the 'case (3:2)'
!  statement.  This is an unreachable CASE because the range is empty.
!
program select_3
  integer i
  do i = 1, 4
     select case(i)
     case (1)
       if (i /= 1) STOP 1
     case (3:2)
       STOP 2
     case (4)
       if (i /= 4) STOP 3
     case default
       if (i /= 2 .and. i /= 3) STOP 4
     end select
  end do
end program select_3
