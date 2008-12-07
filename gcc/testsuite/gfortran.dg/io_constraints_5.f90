! { dg-do compile }
! PR 38425 I/O: POS= compile-time diagnostics
!----------------------------------------------------------
character(len=30) :: str
open(3,access='stream')

! C919 (R913) If io-unit is not a file-unit-number, the
! io-control-spec-list shall not contain a REC= specifier
! or a POS= specifier.
write(str,*, pos=4) 5 ! { dg-error "incompatible with internal" }

! C927 (R913) If a POS= specifier appears, the
! io-control-spec-list shall not contain a REC= specifier.
write(3,pos=5,rec=4) 5 ! { dg-error "POS= is not allowed with REC=" }
write(3,rec=4,pos=5) 5 ! { dg-error "POS= is not allowed with REC=" }

!Fortran runtime error: REC=specifier not allowed with STREAM access
write(3,rec=4) 5
!Fortran runtime error: REC=specifier must be positive
write(3,rec=-3) 44
!Fortran runtime error: POS=specifier must be positive
write(3,pos=-4) 44
end
