! { dg-do compile }
!
! PR fortran/46484
!

function g()
  implicit none
  integer, allocatable :: g
  call int()
    print *, loc(g) ! OK
contains
  subroutine int()
    print *, loc(g) ! OK
    print *, allocated(g) ! OK
  end subroutine int
end function

implicit none
integer, allocatable :: x
print *, allocated(f) ! { dg-error "must be a variable" }
print *, loc(f) ! OK
contains
function f()
  integer, allocatable :: f
  print *, loc(f) ! OK
  print *, allocated(f) ! OK
end function
end
