! { dg-do compile }
! Check that a pointer cannot be a member of a namelist
program namelist_3
  integer,pointer :: x
  allocate (x)
  namelist /n/ x ! { dg-error "NAMELIST attribute conflicts with POINTER attribute" "" }
end program namelist_3
