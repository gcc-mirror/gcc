! { dg-compile }
! PR 31919:  Tests for different ranks in min/max were missing.
program pr31919
  integer :: i4a(2, 2), i4b(2), i4c(4)
  real(4) :: r4a(2, 2), r4b(2), r4c(4)
  real(8) :: r8a(2, 2), r8b(2), r8c(4)

  i4a = max(i4a, i4b)            ! { dg-error "Incompatible ranks" }
  i4a = max0(i4a, i4b)           ! { dg-error "Incompatible ranks" }
  r4a = amax0(i4a, i4b)          ! { dg-error "Incompatible ranks" }
  i4a = max1(r4a, r4b)           ! { dg-error "Incompatible ranks" }
  r4a = amax1(r4a, r4b)          ! { dg-error "Incompatible ranks" }
  r8a = dmax1(r8a, r8b)          ! { dg-error "Incompatible ranks" }

  i4a = min(i4a, i4b)            ! { dg-error "Incompatible ranks" }
  i4a = min0(i4a, i4b)           ! { dg-error "Incompatible ranks" }
  i4a = amin0(i4a, i4b)          ! { dg-error "Incompatible ranks" }
  r4a = min1(r4a, r4b)           ! { dg-error "Incompatible ranks" }
  r4a = amin1(r4a, r4b)          ! { dg-error "Incompatible ranks" }
  r8a = dmin1(r8a, r8b)          ! { dg-error "Incompatible ranks" }

  i4a = max(i4b, i4c)            ! { dg-error "different shape for arguments" }
  i4a = max0(i4b, i4c)           ! { dg-error "different shape for arguments" }
  r4a = amax0(i4b, i4c)          ! { dg-error "different shape for arguments" }
  i4a = max1(r4b, r4c)           ! { dg-error "different shape for arguments" }
  r4a = amax1(r4b, r4c)          ! { dg-error "different shape for arguments" }
  r8a = dmax1(r8B, r8c)          ! { dg-error "different shape for arguments" }

  i4a = min(i4b, i4c)            ! { dg-error "different shape for arguments" }
  i4a = min0(i4b, i4c)           ! { dg-error "different shape for arguments" }
  i4a = amin0(i4b, i4c)          ! { dg-error "different shape for arguments" }
  r4a = min1(r4b, r4c)           ! { dg-error "different shape for arguments" }
  r4a = amin1(r4b, r4c)          ! { dg-error "different shape for arguments" }
  r8a = dmin1(r8b, r8c)          ! { dg-error "different shape for arguments" }
end program
