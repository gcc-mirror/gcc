! { dg-do compile }
! PR fortran/104227 - ICE virtual memory exhausted
! Contributed by G.Steinmetz

program p
  type t
  end type
  type(t) :: x(2)
  print *, transfer(1, x) ! { dg-error "shall not have storage size 0" }
  x = transfer(1, x)      ! { dg-error "shall not have storage size 0" }
end
