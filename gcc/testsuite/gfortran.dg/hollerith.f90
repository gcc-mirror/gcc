! { dg-do run }
! PR15966, PR18781 & PR16531
implicit none
complex(kind=8) x(2) 
complex a(2,2)
character(4) z
character z1(4)
character(4) z2(2,2)
character(80) line
integer i
integer j
real r
character(8) c

data x /16Habcdefghijklmnop, 16Hqrstuvwxyz012345/
data a /8H(i3),abc, 0, 4H(i4), 8H    (i9)/
data z/4h(i5)/
data z1/1h(,1hi,1h6,1h)/
data z2/4h(i7),'xxxx','xxxx','xxxx'/

z2 (1,2) = 4h(i8)
i = 4hHell
j = 4Ho wo
r = 4Hrld! 
write (line, '(3A4)') i, j, r
if (line .ne. 'Hello world!') STOP 1
i = 2Hab
j = 2Hab
r = 2Hab
c = 2Hab
write (line, '(3A4, 8A)') i, j, r, c
if (line .ne. 'ab  ab  ab  ab      ') STOP 2

write(line, '(4A8, "!")' ) x
if (line .ne. 'abcdefghijklmnopqrstuvwxyz012345!') STOP 3

write (line, a) 3
if (line .ne. '  3') STOP 4
write (line, a (1,2)) 4
if (line .ne. '   4') STOP 5
write (line, z) 5
if (line .ne. '    5') STOP 6
write (line, z1) 6
if (line .ne. '     6') STOP 7
write (line, z2) 7
if (line .ne. '      7') STOP 8
write (line, z2 (1,2)) 8
if (line .ne. '       8') STOP 9
write (line, '(16A)') z2
if (line .ne. '(i7)xxxx(i8)xxxx') STOP 10
call test (8h   hello)
end

subroutine test (h)
integer(kind=8) h
character(80) line

write (line, '(8a)') h
if (line .ne. '   hello') STOP 11
end subroutine

! { dg-warning "Hollerith constant" "const" { target *-*-* } 15 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 15 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 16 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 16 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 17 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 18 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 19 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 21 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 21 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 22 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 22 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 23 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 23 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 24 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 24 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 27 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 27 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 28 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 28 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 29 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 29 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 30 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 30 }

! { dg-warning "Non-character in FORMAT tag" "" { target *-*-* } 37 }

! { dg-warning "Non-character in FORMAT tag" "" { target *-*-* } 39 }

! { dg-warning "Hollerith constant" "" { target *-*-* } 51 }
