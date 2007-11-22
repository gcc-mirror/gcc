! { dg-do run }
! { dg-additional-sources bind_c_usage_10_c.c }
!
! PR fortran/34079
!
! Check BIND(C) for ENTRY
!
module mod
 use iso_c_binding
 implicit none
contains
  subroutine sub1(j) bind(c, name="mySub1")
    integer(c_int) :: j
    real(c_float)  :: x
    j = 5
    return
   entry sub1ent(x)
     x = 55.0
  end subroutine sub1
  subroutine sub2(j)
    integer(c_int) :: j
    real(c_float)  :: x
    j = 6
    return
   entry sub2ent(x) bind(c, name="mySubEnt2")
    x = 66.0
  end subroutine sub2
  subroutine sub3(j) bind(c, name="mySub3")
    integer(c_int) :: j
    real(c_float)  :: x
    j = 7
    return
   entry sub3ent(x) bind(c, name="mySubEnt3")
     x = 77.0
  end subroutine sub3
  subroutine sub4(j)
    integer(c_int) :: j
    real(c_float)  :: x
    j = 8
    return
   entry sub4ent(x) bind(c)
     x = 88.0
  end subroutine sub4

  integer(c_int) function func1() bind(c, name="myFunc1")
    real(c_float) :: func1ent
    func1 = -5
    return
   entry func1ent()
    func1ent = -55.0
  end function func1
  integer(c_int) function func2()
    real(c_float) :: func2ent
    func2 = -6
    return
   entry func2ent() bind(c, name="myFuncEnt2")
    func2ent = -66.0
  end function func2
  integer(c_int) function func3() bind(c, name="myFunc3")
    real(c_float) :: func3ent
    func3 = -7
    return
   entry func3ent() bind(c, name="myFuncEnt3")
    func3ent = -77.0
  end function func3
  integer(c_int) function func4()
    real(c_float) :: func4ent
    func4 = -8
    return
   entry func4ent() bind(c)
    func4ent = -88.0
  end function func4
end module mod

! { dg-final { cleanup-modules "mod" } }
