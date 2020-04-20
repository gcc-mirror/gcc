! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/93364 - check fix for ICE in gfc_set_array_spec

type(t) function f()
  codimension :: t[1,2,1,2,1,2,1,*]
  dimension :: t(1,2,1,2,1,2,1,2)
end

! { dg-error "has not been declared" " " { target *-*-* } 6 }
! { dg-error "is of type 't'" " " { target *-*-* } 6 }
! { dg-error "rank \\+ corank of" " " { target *-*-* } 8 }
