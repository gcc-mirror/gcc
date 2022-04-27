! { dg-do compile }
!
! PR fortran/105381
! Infinite recursion with array references of character dummy arguments.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

MODULE m
  implicit none
  integer,  parameter :: ncrit  =  8
  integer,  parameter :: nterm  =  7
contains

  subroutine new_thin_rule (rule1)
    character(*),intent(in) ,optional :: rule1(ncrit)
    character(len=8) :: rules (ncrit,nterm)
    rules = ''
    if (present (rule1)) rules(:,1) = rule1  ! <-- compile time hog
  end subroutine new_thin_rule

end module m
