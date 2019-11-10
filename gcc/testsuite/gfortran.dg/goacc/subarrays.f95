! { dg-do compile }
program test
  implicit none
  integer :: a(10), b(10, 10), c(3:7), i

  !$acc parallel copy(a(1:5))
  !$acc end parallel
  !$acc parallel copy(a(1 + 0 : 5 + 2))
  !$acc end parallel
  !$acc parallel copy(a(:3))
  !$acc end parallel
  !$acc parallel copy(a(3:))
  !$acc end parallel
  !$acc parallel copy(a(:))
  !$acc end parallel
  !$acc parallel copy(a(2:3,2:3))
  ! { dg-error "Rank mismatch" "" { target *-*-* } .-1 }
  ! { dg-error "'a' in MAP clause" "" { target *-*-* } .-2 }
  !$acc end parallel
  !$acc parallel copy (a(:11)) ! { dg-warning "Upper array reference" }
  !$acc end parallel
  !$acc parallel copy (a(i:))
  !$acc end parallel

  !$acc parallel copy (a(:b))
  ! { dg-error "Array index" "" { target *-*-* } .-1 }
  ! { dg-error "'a' in MAP clause" "" { target *-*-* } .-2 }
  !$acc end parallel

  !$acc parallel copy (b(1:3,2:4)) ! { dg-error "Array is not contiguous" }
  !$acc end parallel
  !$acc parallel copy (b(2:3))
  ! { dg-error "Rank mismatch" "" { target *-*-* } .-1 }
  ! { dg-error "'b' in MAP clause" "" { target *-*-* } .-2 }
  !$acc end parallel
  !$acc parallel copy (b(1:, 4:6))
  !$acc end parallel

  !$acc parallel copy (c(2:)) ! { dg-warning "Lower array reference" }
  !$acc end parallel


  !$acc serial copy(a(1:5))
  !$acc end serial
  !$acc serial copy(a(1 + 0 : 5 + 2))
  !$acc end serial
  !$acc serial copy(a(:3))
  !$acc end serial
  !$acc serial copy(a(3:))
  !$acc end serial
  !$acc serial copy(a(:))
  !$acc end serial
  !$acc serial copy(a(2:3,2:3))
  ! { dg-error "Rank mismatch" "" { target *-*-* } .-1 }
  ! { dg-error "'a' in MAP clause" "" { target *-*-* } .-2 }
  !$acc end serial
  !$acc serial copy (a(:11)) ! { dg-warning "Upper array reference" }
  !$acc end serial
  !$acc serial copy (a(i:))
  !$acc end serial

  !$acc serial copy (a(:b))
  ! { dg-error "Array index" "" { target *-*-* } .-1 }
  ! { dg-error "'a' in MAP clause" "" { target *-*-* } .-2 }
  !$acc end serial

  !$acc serial copy (b(1:3,2:4)) ! { dg-error "Array is not contiguous" }
  !$acc end serial
  !$acc serial copy (b(2:3))
  ! { dg-error "Rank mismatch" "" { target *-*-* } .-1 }
  ! { dg-error "'b' in MAP clause" "" { target *-*-* } .-2 }
  !$acc end serial
  !$acc serial copy (b(1:, 4:6))
  !$acc end serial

  !$acc serial copy (c(2:)) ! { dg-warning "Lower array reference" }
  !$acc end serial

end program test
