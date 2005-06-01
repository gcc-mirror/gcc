! PR fortran/21729
! { dg-do compile }
function f1 ()	! { dg-error "has no IMPLICIT type" "f1" }
	implicit none
end function f1
function f2 () result (r2) ! { dg-error "has no IMPLICIT type" "r2" }
	implicit none
end function f2
function f3 ()	! { dg-error "has no IMPLICIT type" "f3" }
	implicit none
entry e3 ()	! { dg-error "has no IMPLICIT type" "e3" }
end function f3
function f4 ()
	implicit none
	real f4
entry e4 ()	! { dg-error "has no IMPLICIT type" "e4" }
end function f4
function f5 ()	! { dg-error "has no IMPLICIT type" "f5" }
	implicit none
entry e5 ()
	real e5
end function f5
