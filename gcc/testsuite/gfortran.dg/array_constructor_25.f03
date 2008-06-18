! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/36492
! Check for incorrect error message with -std=f2003.
! Reduced test based on the one from comment #4, PR 36492.

type t
  character (2) :: arr (1) = [ "a" ]
end type t

end
