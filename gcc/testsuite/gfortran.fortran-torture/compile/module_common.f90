! We were incorrectly trying to create a variable for the common block itself,
! in addition to the variables it contains.
module FOO
  implicit none
  integer I
  common /C/I
contains
  subroutine BAR
  end subroutine BAR
end module FOO
