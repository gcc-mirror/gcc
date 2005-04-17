! { dg-do compile }
! Check that private entities in public namelists are rejected
module namelist_1
  public
  integer,private :: x
  namelist /n/ x ! { dg-error "cannot be member of PUBLIC namelist" "" }
end module
