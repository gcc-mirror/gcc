! { dg-do compile }
program main
  type Xx ! { dg-error "Symbol 'xx' at .1. also declared as a type at .2." }
  end type Xx
  real :: Xx  ! { dg-error "Symbol 'xx' at .1. also declared as a type at .2." }
  
end program main
