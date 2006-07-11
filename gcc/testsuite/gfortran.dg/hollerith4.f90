! { dg-do run }
! Test Hollerith constants assigned to allocatable array
! and used in I/O list.

integer, allocatable :: c (:,:)
character (len = 20) ch
allocate (c(1,2))

c(1,1) = 4H(A4)
c(1,2) = 4H(A5)

write (ch, "(2A4)") c
if (ch .ne. "(A4)(A5)") call abort()
write (ch, c) 'Hello'
if (ch .ne. "Hell") call abort()
write (ch, c (1,2)) 'Hello'
if (ch .ne. "Hello") call abort()

write (ch, *) 5Hhello
if (ch .ne. " hello") call abort()
write (ch, "(A5)") 5Hhello
if (ch .ne. "hello") call abort()

end

! { dg-warning "Hollerith constant" "const" { target *-*-* } 9 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 9 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 10 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 10 }

! { dg-warning "Non-character in FORMAT tag" "" { target *-*-* } 14 }

! { dg-warning "Non-character in FORMAT tag" "" { target *-*-* } 16 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 19 }
! { dg-warning "Hollerith constant" "const" { target *-*-* } 21 }

