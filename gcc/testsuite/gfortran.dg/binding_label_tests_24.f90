! { dg-do compile }
!
! PR fortran/48858
! PR fortran/55465
!
! Was rejected before but it perfectly valid
!
module m
  interface
    subroutine f() bind(C, name="func")
    end subroutine
  end interface
contains
  subroutine sub()
     call f()
  end subroutine
end module m

module m2
  interface
    subroutine g() bind(C, name="func")
    end subroutine
  end interface
contains
  subroutine sub2()
     call g()
  end subroutine
end module m2
