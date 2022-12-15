! { dg-do compile }
! PR fortran/107577 - ICE in find_array_spec
! Contributed by G.Steinmetz

program p
  implicit none
  associate (y => f(4))            ! { dg-error "has no IMPLICIT type" }
    if (lbound (y, 1) /= 1) stop 1 ! { dg-error "Invalid array reference" }
    if (y(1) /= 1) stop 2          ! { dg-error "Invalid array reference" }
  end associate
end

! { dg-error "has no type" " " { target *-*-* } 7 }
