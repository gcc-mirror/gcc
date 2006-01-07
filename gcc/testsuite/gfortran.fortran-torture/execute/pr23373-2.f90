program main
  implicit none
  real, dimension (:), pointer :: x
  x => null ()
  x => test ()
  if (.not. associated (x)) call abort
  if (size (x) .ne. 10) call abort
contains
  function test()
    real, dimension (:), pointer :: test
    if (associated (x)) call abort
    allocate (test (10))
    if (associated (x)) call abort
  end function test
end program main
