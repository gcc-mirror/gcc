! { dg-do compile }
! { dg-options "-Wunused-label" }
!
! PR 128316

module m1
10 integer :: x  ! { dg-error "(1)" }
! { dg-warning "defined but cannot be used" "" { target *-*-* } .-1 }
contains
  subroutine s
  end subroutine
10 end module m1 ! { dg-error "Duplicate statement label 10" }

module m2
  interface
    module subroutine s
    end subroutine
  end interface
end module m2

submodule (m2) sm
20 integer :: y  ! { dg-error "(1)" }
! { dg-warning "defined but cannot be used" "" { target *-*-* } .-1 }
contains
  module subroutine s
  end subroutine
20 end submodule sm ! { dg-error "Duplicate statement label 20" }

module m3
contains
  subroutine s
  end subroutine
30 end module m3 ! { dg-warning "defined but cannot be used" }
