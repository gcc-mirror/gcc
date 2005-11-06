! { dg-do compile }
! PR18082 - Compiler would get stuck in loop, whilst treating
! the assignments.
! Test is one of PR cases.
subroutine snafu (i)
character*(i) :: c1, c2
c1 = ""
c2 = ""
end subroutine snafu


