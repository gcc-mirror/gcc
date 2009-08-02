! { dg-do compile }
! { dg-options "-fall-intrinsics -std=f95" }
! PR15966, PR18781 & PR16531
implicit none
complex(kind=8) x(2) 
complex a(2,2)
character(4) z
character z1(4)
character(4) z2(2,2)
character(80) line
integer i
logical l
real r
character(8) c

data x /16Habcdefghijklmnop, 16Hqrstuvwxyz012345/
data a /8H(i3),abc, 0, 4H(i4), 8H    (i9)/
data z/4h(i5)/
data z1/1h(,1hi,1h6,1h)/
data z2/4h(i7),'xxxx','xxxx','xxxx'/

z2 (1,2) = 4h(i8)
i = 4hHell
l = 4Ho wo
r = 4Hrld! 
write (line, '(3A4)') i, l, r
if (line .ne. 'Hello world!') call abort
i = 2Hab
r = 2Hab
l = 2Hab
c = 2Hab
write (line, '(3A4, 8A)') i, l, r, c
if (line .ne. 'ab  ab  ab  ab      ') call abort

write(line, '(4A8, "!")' ) x
if (line .ne. 'abcdefghijklmnopqrstuvwxyz012345!') call abort

write (line, a) 3
if (line .ne. '  3') call abort
write (line, a (1,2)) 4
if (line .ne. '   4') call abort
write (line, z) 5
if (line .ne. '    5') call abort
write (line, z1) 6
if (line .ne. '     6') call abort
write (line, z2) 7
if (line .ne. '      7') call abort
write (line, z2 (1,2)) 8
if (line .ne. '       8') call abort
write (line, '(16A)') z2
if (line .ne. '(i7)xxxx(i8)xxxx') call abort
call test (8h   hello)
end

subroutine test (h)
integer(kind=8) h
character(80) line

write (line, '(8a)') h
if (line .ne. '   hello') call abort
end subroutine

! { dg-error "Hollerith constant" "const" { target *-*-* } 16 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 17 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 18 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 19 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 20 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 22 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 23 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 24 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 25 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 28 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 29 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 30 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 31 }

! { dg-error "Hollerith constant" "const" { target *-*-* } 52 }

! { dg-error "Non-character in FORMAT tag" "" { target *-*-* } 38 }

! { dg-error "Non-character in FORMAT tag" "" { target *-*-* } 40 }
