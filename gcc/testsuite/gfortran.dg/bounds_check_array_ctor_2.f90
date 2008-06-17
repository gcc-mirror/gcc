! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
!
! PR 36112
! Check correct bounds-checking behaviour for character-array-constructors.

  call test ("this is long")
contains
  subroutine test(s)
    character(len=*) :: s
    character(len=128) :: arr(2)
    arr = (/ "abc", s /)
  end subroutine test
end
! { dg-output "Different CHARACTER lengths \\(3/12\\) in array constructor" }
