! { dg-do compile }
! Check that variable with intent(in) cannot be a member of a namelist
subroutine namelist_2(x)
  integer,intent(in) :: x
  namelist /n/ x
  read(*,n) ! { dg-error "is INTENT" }
end subroutine namelist_2
