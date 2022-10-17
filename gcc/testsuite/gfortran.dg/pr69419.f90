! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/69419 - ICE on invalid coarray in common

blockdata b
   real x           ! { dg-error "must be in COMMON" }
   common /c/ x[*]  ! { dg-error "cannot be a coarray" }
   data x /1.0/
end
