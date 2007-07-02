! { dg-do compile }
module x
  use iso_c_binding
  bind(c) :: test, sub1 ! { dg-error "only be used for variables or common blocks" }
  bind(c) :: sub2 ! { dg-error "only be used for variables or common blocks" }
contains
  function foo() bind(c,name="xx")
    integer(c_int),bind(c,name="xy") :: foo ! { dg-error "only be used for variables or common blocks" }
    ! NAG f95: "BIND(C) for non-variable FOO"  
    ! g95: "Duplicate BIND attribute specified"
    ! gfortran: Accepted
    foo = 5_c_int
  end function foo

  function test()
    integer(c_int) :: test
    bind(c,name="kk") :: test ! { dg-error "only be used for variables or common blocks" }
    ! NAG f95: "BIND(C) for non-variable TEST"
    ! gfortran, g95: Accepted
    test = 5_c_int
  end function test

  function bar() bind(c)
    integer(c_int) :: bar 
    bind(c,name="zx") :: bar ! { dg-error "only be used for variables or common blocks" }
    bar = 5_c_int
  end function bar

  subroutine sub0() bind(c)
    bind(c) :: sub0 ! { dg-error "only be used for variables or common blocks" }
  end subroutine sub0

  subroutine sub1(i) bind(c)
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: i
  end subroutine sub1

  subroutine sub2(i) 
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: i
  end subroutine sub2

  subroutine sub3(i) 
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: i
    bind(c) :: sub3 ! { dg-error "only be used for variables or common blocks" }
  end subroutine sub3
end module x
