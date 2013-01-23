! { dg-do compile }
!
! PR fortran/34899
!
! Allow <tab>1 to <tab>9 as continuation marker, which is a very common
! vendor extension.
!
	PARAMETER (LUMIN=11,LUMAX=20,MAPMAX=256,NPLANEMAX=999)
	INTEGER NAXIS(0:MAPMAX,LUMIN:LUMAX),NAXIS1(0:MAPMAX,LUMIN:LUMAX),
	1NAXIS2(0:MAPMAX,LUMIN:LUMAX),NAXIS3(0:MAPMAX,LUMIN:LUMAX)
	end
! { dg-warning "Nonconforming tab character in column 1 of line 8" "Nonconforming tab" { target *-*-* } 0 }
! { dg-warning "Nonconforming tab character in column 1 of line 9" "Nonconforming tab" { target *-*-* } 0 }
! { dg-warning "Nonconforming tab character in column 1 of line 10" "Nonconforming tab" { target *-*-* } 0 }
! { dg-warning "Nonconforming tab character in column 1 of line 11" "Nonconforming tab" { target *-*-* } 0 }
