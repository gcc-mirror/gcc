! { dg-do run }
!  Short test program with a CASE statement that uses a range.
!
program select_4
  integer i
  do i = 1, 40, 4
     select case(i)
     case (:5)
       if (i /= 1 .and. i /= 5) call abort
     case (20:30)
       if (i /= 21 .and. i /= 25 .and. i /= 29) call abort
     case (34:)
       if (i /= 37) call abort
     end select
  end do
end program select_4
