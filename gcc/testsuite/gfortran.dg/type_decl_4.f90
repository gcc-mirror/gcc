! { dg-do compile }
program main
  type Xx 
  end type Xx
  real :: Xx ! { dg-error "Symbol 'xx' at .* also declared as a type" }
  
end program main
