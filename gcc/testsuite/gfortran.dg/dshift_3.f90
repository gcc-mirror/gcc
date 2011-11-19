! { dg-do compile }
! PR fortran/50753
subroutine foo(i, j, k)

   implicit none

   integer(4), intent(in) :: i, j
   integer(8), intent(in) :: k

   print *, dshiftl(i,      j, 134)     ! { dg-error "must be less than or equal" }
   print *, dshiftl(z'FFF', j, 134)     ! { dg-error "must be less than or equal" }
   print *, dshiftl(i, j, -10)          ! { dg-error "must be nonnegative" }
   print *, dshiftl(z'FFF', z'EEE', 10) ! { dg-error "cannot both be" }
   print *, dshiftl(z'FFF', j, 10)
   print *, dshiftl(i, z'EEE', 10)
   print *, dshiftl(i, j, 10)
   print *, dshiftl(i, k, 10)           ! { dg-error "must be the same type and kind" }
   print *, dshiftl(k, j, 10)           ! { dg-error "must be the same type and kind" }
   print *, dshiftl(i, j, k)
   print *, dshiftl(i, j, z'd')

   print *, dshiftr(i,      j, 134)     ! { dg-error "must be less than or equal" }
   print *, dshiftr(z'FFF', j, 134)     ! { dg-error "must be less than or equal" }
   print *, dshiftr(i, j, -10)          ! { dg-error "must be nonnegative" }
   print *, dshiftr(z'FFF', z'EEE', 10) ! { dg-error "cannot both be" }
   print *, dshiftr(z'FFF', j, 10)
   print *, dshiftr(i, z'EEE', 10)
   print *, dshiftr(i, j, 10)
   print *, dshiftr(i, k, 10)           ! { dg-error "must be the same type and kind" }
   print *, dshiftr(k, j, 10)           ! { dg-error "must be the same type and kind" }
   print *, dshiftr(i, j, k)
   print *, dshiftr(i, j, z'd')

end subroutine foo
