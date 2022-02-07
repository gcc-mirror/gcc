! { dg-do compile }
! PR fortran/104311 - ICE out of memory
! Contributed by G.Steinmetz

program p
  type t
  end type
  type(t) :: x(2)
  print *, transfer(1,x,2)       ! { dg-error "shall not have storage size 0" }
  print *, transfer(1,x,huge(1)) ! { dg-error "shall not have storage size 0" }
end
