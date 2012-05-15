! { dg-do compile }
!
! PR fortran/34657
!
module test_mod
interface
  subroutine my_sub (a)
    real a
  end subroutine
end interface
end module

subroutine my_sub (a)
  use test_mod, gugu => my_sub
  real a
  print *, a
end subroutine

END
