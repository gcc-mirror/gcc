! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
!
! PR 36112
! Check correct bounds-checking behavior for character-array-constructors.

  call test ("short")
contains
  subroutine test(s)
    character(len=*) :: s
    character(len=128) :: arr(3)
    arr = (/ s, "this is long", "this one too" /)
  end subroutine test
end
! { dg-output "Different CHARACTER lengths \\(5/12\\) in array constructor" }
