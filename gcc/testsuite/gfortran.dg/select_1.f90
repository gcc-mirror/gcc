! { dg-do run }
!  Simple test for SELECT CASE
!
program select_2
  integer i
  do i = 1, 5
     select case(i)
     case (1)
       if (i /= 1) call abort
     case (2:3)
       if (i /= 2 .and. i /= 3) call abort
     case (4)
       if (i /= 4) call abort
     case default
       if (i /= 5) call abort
     end select
  end do
end program select_2
