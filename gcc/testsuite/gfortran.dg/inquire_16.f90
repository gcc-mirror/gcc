! { dg-do run }
!
! PR fortran/60286
!
! Contributed by  Alexander Vogt
!
program test_inquire
  use, intrinsic :: ISO_Fortran_env
  implicit none
  character(len=20) :: s_read, s_write, s_readwrite

  inquire(unit=input_unit, read=s_read, write=s_write, &
          readwrite=s_readwrite)
  if (s_read /= "YES" .or. s_write /= "NO" .or. s_readwrite /="NO") then
    STOP 1
  endif

  inquire(unit=output_unit, read=s_read, write=s_write, &
          readwrite=s_readwrite)
  if (s_read /= "NO" .or. s_write /= "YES" .or. s_readwrite /="NO") then
    STOP 2
  endif

  inquire(unit=error_unit, read=s_read, write=s_write, &
          readwrite=s_readwrite)
  if (s_read /= "NO" .or. s_write /= "YES" .or. s_readwrite /="NO") then
    STOP 3
  endif
end program test_inquire
