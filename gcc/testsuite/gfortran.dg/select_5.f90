! { dg-do run }
! Test mismatched type kinds in a select statement.
program select_5
  integer(kind=1) i          ! kind = 1, -128 <= i < 127
  do i = 1, 3
    select case (i)

    ! kind = 4, reachable
    case (1_4)
      if (i /=  1_4) STOP 1

    ! kind = 8, reachable
    case (2_8)
      if (i /= 2_8) STOP 2

    ! kind = 4, unreachable because of range of i
    case (200)                       ! { dg-warning "not in the range" }
      STOP 3

    case default
      if (i /= 3) STOP 4
    end select
  end do
end program select_5
