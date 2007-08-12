! Test various intrinsics who take a kind argument since Fortran 2003
!
! { dg-do compile }
!
program test
  integer, parameter :: k = kind(0)
  logical :: l_array(4,5)
  character(len=1) :: s
  character(len=20) :: t

  l_array = .true.
  s = "u"
  t = "bartutugee"

  call check (count(l_array, kind=k), 20)
  if (any (count(l_array, 2, kind=k) /= 5)) call abort
  if (any (count(l_array, kind=k, dim=2) /= 5)) call abort

  call check (iachar (s, k), 117)
  call check (iachar (s, kind=k), 117)
  call check (ichar (s, k), 117)
  call check (ichar (s, kind=k), 117)

  if (achar(107) /= achar(107,1)) call abort

  call check (index (t, s, .true., k), 7)
  call check (index (t, s, kind=k, back=.false.), 5)

  if (any (lbound (l_array, kind=k) /= 1)) call abort
  call check (lbound (l_array, 1), 1)
  call check (lbound (l_array, 1, kind=k), 1)

  if (any (ubound (l_array, kind=k) /= (/4, 5/))) call abort
  call check (ubound (l_array, 1), 4)
  call check (ubound (l_array, 1, kind=k), 4)

  call check (len(t, k), 20)
  call check (len_trim(t, k), 10)

  call check (scan (t, s, .true., k), 7)
  call check (scan (t, s, kind=k, back=.false.), 5)

  call check (size (l_array, 1, kind=k), 4)
  call check (size (l_array, kind=k), 20)

  call check (verify (t, s, .true., k), 20)
  call check (verify (t, s, kind=k, back=.false.), 1)

contains

  subroutine check(x,y)
    integer, intent(in) :: x, y
    if (x /= y) call abort
  end subroutine check

end program test
