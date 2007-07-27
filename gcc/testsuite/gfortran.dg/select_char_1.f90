integer function char_select (s)
  character(len=*), intent(in) :: s

  select case(s)
    case ("foo")
      char_select = 1
    case ("bar", "gee")
      char_select = 2
    case ("111", "999")
      char_select = 3
    case ("1024", "1900")
      char_select = 4
    case ("12", "17890")
      char_select = 5
    case default
      char_select = -1
  end select
end function char_select

integer function char_select2 (s)
  character(len=*), intent(in) :: s

  char_select2 = -1
  select case(s)
    case ("foo")
      char_select2 = 1
    case ("bar", "gee")
      char_select2 = 2
    case ("111", "999")
      char_select2 = 3
    case ("1024", "1900")
      char_select2 = 4
    case ("12", "17890")
      char_select2 = 5
  end select
end function char_select2


program test
  interface
    integer function char_select (s)
      character(len=*), intent(in) :: s
    end function char_select
    integer function char_select2 (s)
      character(len=*), intent(in) :: s
    end function char_select2
  end interface

  if (char_select("foo") /= 1) call abort
  if (char_select("foo ") /= 1) call abort
  if (char_select("foo2 ") /= -1) call abort
  if (char_select("bar") /= 2) call abort
  if (char_select("gee") /= 2) call abort
  if (char_select("000") /= -1) call abort
  if (char_select("101") /= -1) call abort
  if (char_select("109") /= -1) call abort
  if (char_select("111") /= 3) call abort
  if (char_select("254") /= -1) call abort
  if (char_select("999") /= 3) call abort
  if (char_select("9989") /= -1) call abort
  if (char_select("1882") /= -1) call abort

  if (char_select2("foo") /= 1) call abort
  if (char_select2("foo ") /= 1) call abort
  if (char_select2("foo2 ") /= -1) call abort
  if (char_select2("bar") /= 2) call abort
  if (char_select2("gee") /= 2) call abort
  if (char_select2("000") /= -1) call abort
  if (char_select2("101") /= -1) call abort
  if (char_select2("109") /= -1) call abort
  if (char_select2("111") /= 3) call abort
  if (char_select2("254") /= -1) call abort
  if (char_select2("999") /= 3) call abort
  if (char_select2("9989") /= -1) call abort
  if (char_select2("1882") /= -1) call abort
end program test
