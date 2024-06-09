! { dg-do run }
! PR99210 X editing for reading file with encoding='utf-8'
program test_bug_format_x
  use iso_fortran_env
  integer, parameter :: u = selected_char_kind('ISO_10646')

  character(kind=u, len=1) a, b, a1, b1, b2

  open(unit=10, file='test_bug_format_x.tmp', encoding='UTF-8')

  a = char(int(z'03B1'), u)
  b = char(int(z'03B2'), u)
  write(10, '(a1, a1)') a, b

  rewind(10)
  read(10, '(a1, a1)') a1, b1

  rewind(10)
  read(10, '(1x, a1)') b2

  close (10, status="delete")
  if(a /= a1 .or. b /= b1) then
    error stop 1
  end if

  if(b /= b2) then
    error stop 2
  end if
end program test_bug_format_x
