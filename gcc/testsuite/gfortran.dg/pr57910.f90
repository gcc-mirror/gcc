! { dg-do run }
program strtest

  implicit none

  character(len=:), allocatable:: my_str

  integer, parameter :: slen_init = 7
  integer :: slen = slen_init

  my_str = fstr(slen)
  if (slen /= slen_init .or. len(my_str) /= slen .or. my_str /= ' ') then
    STOP 1
  endif

contains

  function fstr(strlen)
    integer, value :: strlen
    character(len=strlen)::fstr

    strlen = 17  ! Make sure strlen was really passed by value
    fstr = ' '
  end function fstr

end program strtest
