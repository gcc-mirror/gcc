! { dg-do compile }
! { dg-options "-std=f95" }
! Check that a pointer cannot be a member of a namelist
program namelist_3
  integer,pointer :: x
  allocate (x)
  namelist /n/ x ! { dg-error "NAMELIST attribute with POINTER attribute" "" }
end program namelist_3
