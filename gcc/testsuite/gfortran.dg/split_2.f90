! { dg-do run }
program b
  integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
  character(kind=ucs4, len=:), allocatable :: input, set
  integer :: p = 0

  input = char(int(z'4f60'), ucs4) // char(int(z'597d'), ucs4) // char(int(z'4f60'), ucs4) // char(int(z'4e16'), ucs4)
  set = char(int(z'597d'), ucs4) // char(int(z'4e16'), ucs4)

  call split(input, set, p)
  if (p /= 2) stop 1
  call split(input, set, p)
  if (p /= 4) stop 2
  call split(input, set, p)
  if (p /= 5) stop 3
  call split(input, set, p, .true.)
  if (p /= 4) stop 4
  call split(input, set, p, .true.)
  if (p /= 2) stop 5
  call split(input, set, p, .true.)
  if (p /= 0) stop 6
end program b
