! { dg-do compile }
!
! PR fortran/34899
!
! Allow <tab>1 to <tab>9 as continuation marker, which is a very common
! vendor extension.
!
! Note: The test suite is run with -pedantic-errors, which both enables
! the tab warning (-pedantic implies -Wtabs) and turns it into errors
! (-pedantic-errors hence implies -Werror=tabs).
!
	PARAMETER (LUMIN=11,LUMAX=20,MAPMAX=256,NPLANEMAX=999)
	INTEGER NAXIS(0:MAPMAX,LUMIN:LUMAX),NAXIS1(0:MAPMAX,LUMIN:LUMAX),
	1NAXIS2(0:MAPMAX,LUMIN:LUMAX),NAXIS3(0:MAPMAX,LUMIN:LUMAX)
	end
! { dg-error "Nonconforming tab character in column 1 of line 12" "Nonconforming tab" { target *-*-* } 0 }
! { dg-error "Nonconforming tab character in column 1 of line 13" "Nonconforming tab" { target *-*-* } 0 }
! { dg-error "Nonconforming tab character in column 1 of line 14" "Nonconforming tab" { target *-*-* } 0 }
! { dg-error "Nonconforming tab character in column 1 of line 15" "Nonconforming tab" { target *-*-* } 0 }
! { dg-prune-output "some warnings being treated as errors" }
