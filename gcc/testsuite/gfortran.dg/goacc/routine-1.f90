! { dg-do compile }

  integer, parameter :: n = 10
  integer :: a(n), i
  integer, external :: fact
  i = 1
  !$acc routine (fact)  ! { dg-error "\\!\\$ACC ROUTINE statement at \\(1\\) cannot appear after executable statements" }
  !$acc routine ()  ! { dg-error "Syntax error in \\\!\\\$ACC ROUTINE \\\( NAME \\\)" }
  !$acc parallel
  !$acc loop
  do i = 1, n
     a(i) = fact (i)
     call incr (a(i))
  end do
  !$acc end parallel
  do i = 1, n
     write (*, "(I10)") a(i)
  end do
end
recursive function fact (x) result (res)
  integer, intent(in) :: x
  integer :: res
  res = 1
  !$acc routine  ! { dg-error "\\!\\$ACC ROUTINE statement at \\(1\\) cannot appear after executable statements" }
  if (x < 1) then
     res = 1
  else
     res = x * fact (x - 1)
  end if
end function fact
subroutine incr (x)
  integer, intent(inout) :: x
  integer i
  i = 0
  !$acc routine  ! { dg-error "\\!\\$ACC ROUTINE statement at \\(1\\) cannot appear after executable statements" }
  x = x + 1
end subroutine incr
