! Compiled with pr83149_1.f90
!
module mod1
  integer :: ncells
end module

module mod2
contains
  function get() result(array)
    use mod1
    real array(ncells)
    array = 1.0
  end function
end module
