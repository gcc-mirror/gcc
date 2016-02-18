! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
!
! PR 36112
! Check correct bounds-checking behavior for character-array-constructors.

  call test ("short", "this is long")
contains
  subroutine test(r, s)
    character(len=*) :: r, s
    character(len=128) :: arr(2)
    arr = (/ r, s /)
  end subroutine test
end
! { dg-output "Different CHARACTER lengths \\(5/12\\) in array constructor" }
