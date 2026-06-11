! { dg-do compile }
!
! PR 125747
!
! Shadowing a 63-character index-name used to hang the compiler.

program p
  implicit none
  integer :: iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  integer :: s
  s = 0
  do concurrent (integer(8) :: iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii = 1:2) reduce(+:s)
    s = s + 1
  end do
  do concurrent (integer(2) :: iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii = 1:3) reduce(+:s)
    s = s + 1
  end do
  if (s /= 5) stop 1
end program p
