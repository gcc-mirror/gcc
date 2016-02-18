! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
!
! PR 36112
! Check correct bounds-checking behavior for character-array-constructors.

  call test ("short", "also5")
contains
  subroutine test(r, s)
    character(len=*) :: r, s
    character(len=128) :: arr(3)
    arr = (/ r, s, "this is too long" /)
  end subroutine test
end
! { dg-output "Different CHARACTER lengths \\(5/16\\) in array constructor" }
