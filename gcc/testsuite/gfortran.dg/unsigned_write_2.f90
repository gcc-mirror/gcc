! { dg-do  run }
! This is a libgfortran (runtime library) test, need to run only once!
!
! { dg-additional-options "-funsigned" }
!
! PR libfortran/118536 - G formatting for UNSIGNED

program print_unsigned_g_formatted
  character(21) :: s1, s2
  unsigned(1)  :: u1 = huge(0U_1)
  unsigned(2)  :: u2 = huge(0U_2)
  unsigned(4)  :: u4 = huge(0U_4)
  unsigned(8)  :: u8 = huge(0U_8)

  write(s1,'(i0)') u1
  write(s2,'(g0)') u1
  if (s1 /= s2) stop 1

  write(s1,'(i0)') u2
  write(s2,'(g0)') u2
  if (s1 /= s2) stop 2

  write(s1,'(i0)') u4
  write(s2,'(g0)') u4
  if (s1 /= s2) stop 3

  write(s1,'(i0)') u8
  write(s2,'(g0)') u8
  if (s1 /= s2) stop 4
end
