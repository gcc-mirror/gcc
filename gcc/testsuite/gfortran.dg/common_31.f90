! { dg-do compile }
! PR fortran/119199 - reject SAVE of a COMMON in a BLOCK construct
!
! F2023:C1108: A SAVE statement in a BLOCK construct shall contain a
!              saved-entity-list that does not specify a common-block-name.
!
! Contributed by David Binderman

program main
  real r
  common /argmnt2/ r
  block
    save /argmnt2/ ! { dg-error "not allowed in a BLOCK construct" }
  end block
end
