! { dg-do run }
!  Simple test for SELECT CASE
!
program select_2
  integer i
  do i = 1, 5
     select case(i)
     case (1)
       if (i /= 1) STOP 1
     case (2:3)
       if (i /= 2 .and. i /= 3) STOP 2
     case (4)
       if (i /= 4) STOP 3
     case default
       if (i /= 5) STOP 4
     end select
  end do
end program select_2
