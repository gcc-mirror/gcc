! { dg-do compile }
!

program test
  procedure(team_num) :: g ! { dg-error "must be explicit" }
end program
