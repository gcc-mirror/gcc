! Compiled with pr83149_b.f90
!
module mod
  character(8) string
contains
  function get_string() result(s)
    character(len_trim(string)) s
    s = string
  end function
end module

