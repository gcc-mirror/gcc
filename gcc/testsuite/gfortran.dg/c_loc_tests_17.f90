! { dg-do compile }
!
! PR fortran/55574
! The following code used to be accepted because C_LOC pulls in C_PTR
! implicitly.
!
! Contributed by Valery Weber <valeryweber@hotmail.com>
!
program aaaa
  use iso_c_binding, only : c_loc
  integer, target :: i
  type(C_PTR) :: f_ptr ! { dg-error "being used before it is defined" }
  f_ptr=c_loc(i)  ! { dg-error "Cannot convert" }
end program aaaa
