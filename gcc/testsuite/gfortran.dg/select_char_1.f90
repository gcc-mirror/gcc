! { dg-do run }
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

  if (char_select("foo") /= 1) STOP 1
  if (char_select("foo ") /= 1) STOP 2
  if (char_select("foo2 ") /= -1) STOP 3
  if (char_select("bar") /= 2) STOP 4
  if (char_select("gee") /= 2) STOP 5
  if (char_select("000") /= -1) STOP 6
  if (char_select("101") /= -1) STOP 7
  if (char_select("109") /= -1) STOP 8
  if (char_select("111") /= 3) STOP 9
  if (char_select("254") /= -1) STOP 10
  if (char_select("999") /= 3) STOP 11
  if (char_select("9989") /= -1) STOP 12
  if (char_select("1882") /= -1) STOP 13

  if (char_select2("foo") /= 1) STOP 14
  if (char_select2("foo ") /= 1) STOP 15
  if (char_select2("foo2 ") /= -1) STOP 16
  if (char_select2("bar") /= 2) STOP 17
  if (char_select2("gee") /= 2) STOP 18
  if (char_select2("000") /= -1) STOP 19
  if (char_select2("101") /= -1) STOP 20
  if (char_select2("109") /= -1) STOP 21
  if (char_select2("111") /= 3) STOP 22
  if (char_select2("254") /= -1) STOP 23
  if (char_select2("999") /= 3) STOP 24
  if (char_select2("9989") /= -1) STOP 25
  if (char_select2("1882") /= -1) STOP 26
end program test
