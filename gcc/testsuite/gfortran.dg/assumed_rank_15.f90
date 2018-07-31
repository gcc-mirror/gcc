! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! PR fortran/83184
!

structure /s/
  integer n(..) /1/ ! { dg-error "must have an explicit shape" }
end structure

end
