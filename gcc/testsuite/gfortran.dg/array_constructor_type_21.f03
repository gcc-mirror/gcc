! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/36492
! Check that it works with a typespec even for not-the-same-length elements.

type t
  character (1) :: arr (2) = [ character(len=2) :: "a", "ab" ]
end type t

end
