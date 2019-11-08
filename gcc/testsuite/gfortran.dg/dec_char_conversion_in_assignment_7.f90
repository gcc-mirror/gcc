! { dg-do run }
! { dg-options "-fdec -Wconversion -Wcharacter-truncation" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
program test
  integer(4), parameter :: a = '1234'
  real(4), parameter :: b = '12'
  complex(4), parameter :: c = '12341234'
  logical(4), parameter :: d = 'abcd'
  integer(4), parameter :: e = 4h1234
  real(4), parameter :: f = 2h12
  complex(4), parameter :: g = 8h12341234
  logical(4), parameter :: h = 4habcd

  if (a.ne.e) stop 1
  if (b.ne.f) stop 2
  if (c.ne.g) stop 3
  if (d.neqv.h) stop 4
end program

! { dg-warning "Nonstandard conversion from CHARACTER" " " { target *-*-* } 7 }
! { dg-warning "Nonstandard conversion from CHARACTER" " " { target *-*-* } 8 }
! { dg-warning "Nonstandard conversion from CHARACTER" " " { target *-*-* } 9 }
! { dg-warning "Nonstandard conversion from CHARACTER" " " { target *-*-* } 10 }
! { dg-warning "Conversion from HOLLERITH to INTEGER" " " { target *-*-* } 11 }
! { dg-warning "Conversion from HOLLERITH to REAL" " " { target *-*-* } 12 }
! { dg-warning "Conversion from HOLLERITH to COMPLEX" " " { target *-*-* } 13 }
! { dg-warning "Conversion from HOLLERITH to LOGICAL" " " { target *-*-* } 14 }

