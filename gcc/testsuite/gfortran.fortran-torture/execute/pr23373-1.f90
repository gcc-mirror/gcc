program main
  implicit none
  real, dimension (:), pointer :: x
  x => null ()
  x => test (x)
  if (.not. associated (x)) STOP 1
  if (size (x) .ne. 10) STOP 2
contains
  function test (p)
    real, dimension (:), pointer :: p, test
    if (associated (p)) STOP 3
    allocate (test (10))
    if (associated (p)) STOP 4
  end function test
end program main
