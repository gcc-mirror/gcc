! { dg-do link }
!
! PR fortran/40571
!
! This test case adds check for the new Fortran 2008 array parameters
! in ISO_FORTRAN_ENV: integer_kinds, logical_kinds, character_kinds,
! and real_kinds.
!
! The test thus also checks that the values of the parameter are used
! and no copy is made. (Cf. PR 44856.)

program test
  use iso_fortran_env, only: integer_kinds, character_kinds
  implicit none
  integer :: aaaa(2),i
  i=1

  print *, integer_kinds
  print *, integer_kinds(1)
  print *, (integer_kinds)
  print *, (integer_kinds + 1)
  print *, integer_kinds(1:2)
  print *, integer_kinds(i)

  aaaa = character_kinds
  aaaa(1:2) = character_kinds(1:2)
  aaaa(i) = character_kinds(i)
  aaaa = character_kinds + 0
  aaaa(1:2) = character_kinds(1:2) + 0
  aaaa(i) = character_kinds(i) + 0
end program test

subroutine one()
  use iso_fortran_env, only: ik => integer_kinds, ik2 => integer_kinds
  implicit none

  if (any (ik /= ik2)) call never_call_me()
end subroutine one

subroutine two()
  use iso_fortran_env
  implicit none

  ! Should be 1, 2, 4, 8 and possibly 16
  if (size (integer_kinds) < 4) call never_call_me()
  if (any (integer_kinds(1:4) /= [1,2,4,8])) call never_call_me()
  if (any (integer_kinds /= logical_kinds)) call never_call_me()

  if (size (character_kinds) /= 2) call never_call_me()
  if (any (character_kinds /= [1,4])) call never_call_me()

  if (size (real_kinds) < 2) call never_call_me()
  if (any (real_kinds(1:2) /= [4,8])) call never_call_me()
end subroutine two

subroutine three()
  use iso_fortran_env
  integer :: i, j(2)
  i = real_kinds(1)
  j = real_kinds(1:2)
end subroutine three
