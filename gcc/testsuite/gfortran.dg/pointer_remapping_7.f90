! { dg-do compile }
!
! PR fortran/49624
!
  integer, target :: A(100)
  integer,pointer :: P(:,:)
  p(10,1:) => A  ! { dg-error "Lower bound has to be present" }
  end
