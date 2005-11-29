! { dg-do compile }

  real :: a
  a = 6.0
  open (unit = 6, file = 'foo', access = a) ! { dg-error "must be of type CHARACTER" }
end
