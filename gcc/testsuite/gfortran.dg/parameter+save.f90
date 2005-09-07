! { dg-do compile }
! PR 20848 - parameter and save should conflict.
  integer, parameter, save :: x=0 ! { dg-error "conflicts" }
  integer, save :: y 
  parameter (y=42)   ! { dg-error "conflicts" }
end
