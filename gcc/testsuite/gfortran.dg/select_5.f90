! { dg-do run }
! Test mismatched type kinds in a select statement.
program select_5
  integer*1 i          ! kind = 1, -128 <= i < 127
  do i = 1, 3
    select case (i)     
    case (1_4)         ! kind = 4, reachable
      if (i /=  1_4) call abort
    case (2_8)         ! kind = 8, reachable
      if (i /= 2_8) call abort
    case (200)         ! kind = 4, unreachable because of range of i
      call abort
    case default
      if (i /= 3) call abort
    end select
  end do
end program select_5
