! { dg-do compile }
! PR fortran/101327 - ICE in find_array_element, at fortran/expr.c:1355

subroutine s
  integer, parameter :: n([2]) = [1, 2] ! { dg-error "must be scalar" }
  type t
     integer :: a(n(1):n(2))
  end type
end

! { dg-error "cannot be automatic or of deferred shape" " " { target *-*-* } 5 }
