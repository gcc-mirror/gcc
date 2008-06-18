! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/36492
! Check that the error is still emitted for really incorrect constructor.

type t
  character (2) :: arr (2) = [ "a", "ab" ] ! { dg-error "Different CHARACTER" }
end type t

end
