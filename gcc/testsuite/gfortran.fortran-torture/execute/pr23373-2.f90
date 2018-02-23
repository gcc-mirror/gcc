program main
  implicit none
  real, dimension (:), pointer :: x
  x => null ()
  x => test ()
  if (.not. associated (x)) STOP 1
  if (size (x) .ne. 10) STOP 2
contains
  function test()
    real, dimension (:), pointer :: test
    if (associated (x)) STOP 3
    allocate (test (10))
    if (associated (x)) STOP 4
  end function test
end program main
