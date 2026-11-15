! { dg-do compile }
!
! PR 128316

17 continue ! { dg-error "(1)" }
contains
  subroutine s
  end subroutine
17 end ! { dg-error "Duplicate statement label 17" }
