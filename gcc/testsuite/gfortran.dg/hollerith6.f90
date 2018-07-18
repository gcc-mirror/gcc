! PR fortran/39865
! { dg-do run }

subroutine foo (a)
  integer(kind=4) :: a(1, 3)
  character(len=40) :: t
  write (t, fmt=a(1,2)) 1, 2, 3, 4, 5, 6, 7, 8
  if (t .ne. '   1   2   3   4   5   6   7   8') STOP 1
end subroutine foo
  interface
    subroutine foo (a)
      integer(kind=4) :: a(1, 3)
    end subroutine foo
  end interface
  integer(kind=4) :: b(1,3)
  character(len=40) :: t
  b(1,1) = 4HXXXX
  b(1,2) = 4H (8I
  b(1,3) = 2H4)
  write (t, fmt=b(1,2)) 1, 2, 3, 4, 5, 6, 7, 8
  if (t .ne. '   1   2   3   4   5   6   7   8') STOP 2
  call foo (b)
end

! { dg-warning "Non-character in FORMAT tag" "FMT" { target *-*-* } 7 }
! { dg-warning "Non-character in FORMAT tag" "FMT" { target *-*-* } 20 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 17 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 17 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 18 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 18 }

! { dg-warning "Hollerith constant" "const" { target *-*-* } 19 }
! { dg-warning "Conversion" "conversion" { target *-*-* } 19 }
