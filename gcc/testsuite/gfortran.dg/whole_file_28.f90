! { dg-do compile }
! Test the fix for the problem described in PR45077 comments #4 and #5.
! Note that the module file is kept for whole_file_29.f90
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module iso_red
  type, public :: varying_string
     character(LEN=1), dimension(:), allocatable :: chars
  end type varying_string
end module iso_red
! DO NOT CLEAN UP THE MODULE FILE - whole_file_29.f90 does it.
