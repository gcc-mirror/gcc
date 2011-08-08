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
  use test_mod ! { dg-error "is also the name of the current program unit" }
  real a
  print *, a
end subroutine


module test_mod2
  integer :: my_sub2
end module

subroutine my_sub2 (a)
  use test_mod2 ! { dg-error "is also the name of the current program unit" }
  real a
  print *, a
end subroutine


subroutine my_sub3 (a)
  use test_mod2, my_sub3 => my_sub2  ! { dg-error "is also the name of the current program unit" }
  real a
  print *, a
end subroutine

END

! { dg-final { cleanup-modules "test_mod test_mod2" } }
