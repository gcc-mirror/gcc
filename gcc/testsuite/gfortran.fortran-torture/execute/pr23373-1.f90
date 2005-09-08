program main
  implicit none
  real, dimension (:), pointer :: x
  x => null ()
  x => test (x)
  if (.not. associated (x)) call abort
  if (size (x) .ne. 10) call abort
contains
  function test (p)
    real, dimension (:), pointer :: p, test
    if (associated (p)) call abort
    allocate (test (10))
    if (associated (p)) call abort
  end function test
end program main
