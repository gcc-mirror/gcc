! { dg-do compile }
! { dg-additional-options "-Wcharacter-truncation -fdump-tree-original" }
! PR fortran/121727

program p
  use iso_c_binding, only: c_char
  implicit none
  call cbind('abcd')   ! { dg-warning "length of actual argument longer" }
  call one  ('efgh')   ! { dg-warning "length of actual argument longer" }
  call one4 (4_'IJKL') ! { dg-warning "length of actual argument longer" }

  call two4 (4_'MNOP') ! { dg-warning "length of actual argument longer" }
  call three('efgh')   ! { dg-warning "length of actual argument longer" }
  call four ('ijklmn') ! { dg-warning "length of actual argument longer" }
contains
  subroutine cbind(c) bind(C)
    character(kind=c_char,len=1), value :: c
  end

  subroutine one(x)
    character(kind=1,len=1), value :: x
  end

  subroutine one4(w)
    character(kind=4,len=1), value :: w
  end

  subroutine two4(y)
    character(kind=4,len=2), value :: y
  end

  subroutine three(z)
    character(kind=1,len=3), value :: z
  end

  subroutine four(v)
    character(kind=1,len=4), optional, value :: v
  end
end

! { dg-final { scan-tree-dump-times "two4 \\(.*, 2\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "three \\(.*, 3\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "four \\(.*, 1, 4\\);" 1 "original" } }
