! PR fortran/39865
! { dg-do compile }

subroutine foo (a)
  integer(kind=4), target :: a(1:, 1:)
  integer(kind=4), pointer :: b(:, :)
  b => a
  write (*, fmt=a(1,2)) 1, 2, 3, 4, 5, 6, 7, 8
  write (*, fmt=b(1,2)) 1, 2, 3, 4, 5, 6, 7, 8
end subroutine foo
subroutine bar (a, b)
  character :: b(2,*)
  integer :: a(*)
  write (*, fmt=b) 1, 2, 3
  write (*, fmt=a) 1, 2, 3
  write (*, fmt=a(2)) 1, 2, 3
end subroutine
  interface
    subroutine foo (a)
      integer(kind=4), target :: a(:, :)
    end subroutine foo
  end interface
  integer(kind=4) :: a(2, 3)
  a = 4HXXXX
  a(2,2) = 4H (8I
  a(1,3) = 2H4)
  a(2,3) = 1H 
  call foo (a(2:2,:))
end

! { dg-warning "Non-character in FORMAT tag" "FMT" { target *-*-* } 8 }
! { dg-error "Non-character assumed shape array element in FORMAT tag" "element" { target *-*-* } 8 }

! { dg-warning "Non-character in FORMAT tag" "FMT" { target *-*-* } 9 }
! { dg-error "Non-character pointer array element in FORMAT tag" "element" { target *-*-* } 9 }

! { dg-error "reference to the assumed size array" "assumed-size" { target *-*-* } 14 }
! { dg-error "reference to the assumed size array" "assumed-size" { target *-*-* } 15 }
! { dg-warning "Non-character in FORMAT tag" "FMT" { target *-*-* } 16 }
! { dg-error "Non-character assumed size array element in FORMAT tag" "element" { target *-*-* } 16 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 24 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 24 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 25 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 25 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 26 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 26 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 27 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 27 }
