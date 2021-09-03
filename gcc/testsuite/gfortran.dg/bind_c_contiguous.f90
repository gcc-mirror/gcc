module m
  use iso_c_binding
  implicit none (type, external)
contains

! All of the following use an array descriptor
! F2018, 18.3.7 (5) applies:

subroutine f1 (x) bind(c)  ! { dg-error "Dummy argument 'x' at .1. may not be a pointer with CONTIGUOUS attribute as procedure 'f1' is BIND\\(C\\)" }
  character(len=:, kind=c_char), pointer, contiguous :: x(:)
end

subroutine f2 (x) bind(c)  ! { dg-error "Dummy argument 'x' at .1. may not be a pointer with CONTIGUOUS attribute as procedure 'f2' is BIND\\(C\\)" }
  integer(c_int), pointer, contiguous :: x(:)
end

subroutine f3 (x) bind(c)
  character(len=:, kind=c_char), pointer :: x(:)  ! OK - pointer but not contiguous
end

subroutine f4 (x) bind(c)
  character(len=*, kind=c_char), contiguous :: x(:)  ! OK - contiguous but not a pointer
end

subroutine f5 (x) bind(c)
  integer(c_int), pointer :: x(:)  !  OK - pointer but not contigous
end

subroutine f6 (x) bind(c)
  integer(c_int), contiguous :: x(:)  !  OK - contiguous but not a pointer
end

end
