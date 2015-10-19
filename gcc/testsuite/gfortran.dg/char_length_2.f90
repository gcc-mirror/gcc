! { dg-do compile }
! Tests the fix for PR 31250.
! The fix for PR fortran/67987 supercedes PR 31250, which removes
! the -Wsurprising option.
!
CHARACTER(len=0) :: c1   ! This is OK.
CHARACTER(len=-1) :: c2
PARAMETER(I=-100)
CHARACTER(len=I) :: c3
CHARACTER(len=min(I,500)) :: c4
CHARACTER(len=max(I,500)) :: d1  ! no warning
CHARACTER(len=5) :: d2   ! no warning

if (len(c1) .ne. 0) call link_error ()
if (len(c2) .ne. len(c1)) call link_error ()
if (len(c3) .ne. len(c2)) call link_error ()
if (len(c4) .ne. len(c3)) call link_error ()

if (len(d1) .ne. 500) call link_error ()
if (len(d2) .ne. 5) call link_error ()
END
