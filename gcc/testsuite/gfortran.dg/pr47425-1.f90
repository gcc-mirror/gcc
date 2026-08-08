! { dg-do compile }
! PR47425
! Test that gfortran does not ICE on array constructors with a character
! type-spec containing a function call.
subroutine pr47425_const(L, s, e)
  implicit none
  character(*), intent(in) :: L
  integer, intent(in) :: s, e
  ! Constant character length: must compile cleanly.
  if (any(L(s:e+1) == [character(5) :: 'that', 'those'])) then
    write (*, *) 'match'
  end if
end subroutine pr47425_const

subroutine pr47425_paramlen(s, e, n)
  implicit none
  integer, intent(in) :: s, e, n
  ! Variable length from a plain integer dummy: must compile cleanly.
  if (any(['that', 'thos'] == [character(n) :: 'that', 'thos'])) then
    write (*, *) 'match'
  end if
end subroutine pr47425_paramlen
