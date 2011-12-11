! { dg-do compile }
! { dg-options "-Wall" }
!
! PR fortran/50923
!
module m
contains
  integer pure function f() ! { dg-warning "Return value of function 'f' at .1. not set" }
  end function f
  integer pure function g() result(h) ! { dg-warning "Return value 'h' of function 'g' declared at .1. not set" }
  end function g
  integer pure function i()
    i = 7
  end function i
  integer pure function j() result(k)
    k = 8
  end function j
end module m
! { dg-final { cleanup-modules "mod" } }
