! { dg-do compile }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>

program test
  use iso_fortran_env
  implicit none
  integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
  integer :: x
  character(len=7) :: s = "abcd123"
  character(4, ucs4) :: s4 = char(int(z'20ac'), ucs4) // ucs4_"100"

  x = s
  x = "string"
  x = "A longer string" // " plus a bit"
  x = s // s
  x = s // "a bit more"
  x = "prefix:" // s
  x = s4
  x = ucs4_"string"
  x = ucs4_"A longer string" // ucs4_" plus a bit"
  x = s4 // s4
  x = s4 // ucs4_"a bit more"
  x = ucs4_"prefix:" // s4

  call f(s)
  call f("string")
  call f("A longer string" // " plus a bit")
  call f(s // s)
  call f(s // "a bit more")
  call f("a string:" // s)

  call f(s4)
  call f(ucs4_"string")
  call f(ucs4_"A longer string" // ucs4_" plus a bit")
  call f(s4 // s4)
  call f(s4 // ucs4_"a bit more")
  call f(ucs4_"a string:" // s4)

  write(*,*) "" // ucs4_""

contains
  subroutine f(y)
    integer, intent(in) :: y

    write(*,*) y
  end subroutine f

end program

! { dg-error "CHARACTER\\(7\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 13 }
! { dg-error "CHARACTER\\(6\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 14 }
! { dg-error "CHARACTER\\(26\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 15 }
! { dg-error "CHARACTER\\(14\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 16 }
! { dg-error "CHARACTER\\(17\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 17 }
! { dg-error "CHARACTER\\(14\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 18 }
! { dg-error "CHARACTER\\(4,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 19 }
! { dg-error "CHARACTER\\(6,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 20 }
! { dg-error "CHARACTER\\(26,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 21 }
! { dg-error "CHARACTER\\(8,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 22 }
! { dg-error "CHARACTER\\(14,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 23 }
! { dg-error "CHARACTER\\(11,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 24 }
! { dg-error "CHARACTER\\(7\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 26 }
! { dg-error "CHARACTER\\(6\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 27 }
! { dg-error "CHARACTER\\(26\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 28 }
! { dg-error "CHARACTER\\(14\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 29 }
! { dg-error "CHARACTER\\(17\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 30 }
! { dg-error "CHARACTER\\(16\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 31 }
! { dg-error "CHARACTER\\(4,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 33 }
! { dg-error "CHARACTER\\(6,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 34 }
! { dg-error "CHARACTER\\(26,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 35 }
! { dg-error "CHARACTER\\(8,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 36 }
! { dg-error "CHARACTER\\(14,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 37 }
! { dg-error "CHARACTER\\(13,4\\) to INTEGER\\(4\\)" "type mismatch" { target \*-\*-\* } 38 }
! { dg-error "CHARACTER\\(0\\)/CHARACTER\\(0,4\\)" "operand type mismatch" { target \*-\*-\* } 40 }

