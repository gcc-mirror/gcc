! { dg-do run }
! Test mismatched type kinds in a select statement.
program select_5
  integer(kind=1) i          ! kind = 1, -128 <= i < 127
  do i = 1, 3
    select case (i)

    ! kind = 4, reachable
    case (1_4)
      if (i /=  1_4) call abort

    ! kind = 8, reachable
    case (2_8)
      if (i /= 2_8) call abort

    ! kind = 4, unreachable because of range of i
    case (200)                       ! { dg-warning "not in the range" }
      call abort

    case default
      if (i /= 3) call abort
    end select
  end do
end program select_5
