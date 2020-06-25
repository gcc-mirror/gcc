! { dg-do compile }
! PR fortran/95503 - ICE in gfc_is_simply_contiguous

program p
  complex, target :: a
  real, pointer, contiguous :: b => a%re ! { dg-error "not an array pointer" }
end
