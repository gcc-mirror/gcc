! { dg-do compile }
! PR fortran/102717

program p
  integer, parameter :: a(1) = 2
  integer, parameter :: b(2) = reshape([3,4], -[a]) ! { dg-error "negative" }
end
