! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Unequal character length" }

! PR fortran/31822
! Verify that runtime checks for matching character length
! in pointer assignment work.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program ptr
  implicit none
  character(len=10), target :: s1
  call bar((/ s1, s1 /))
contains
  subroutine bar(s)
    character(len=*),target  :: s(2)
    character(len=17),pointer :: p(:)
    p => s
  end subroutine bar
end program ptr

! { dg-output "Unequal character lengths \\(17/10\\)" }
