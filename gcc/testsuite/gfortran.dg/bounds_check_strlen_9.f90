! { dg-do run }
! { dg-options "-fbounds-check" }
!
! PR fortran/40452
! The following program is valid Fortran 90 and later.
! The storage-sequence association of the dummy argument
! allows that the actual argument ["ab", "cd"] is mapped
! to the dummy argument a(1) which perfectly fits.
! (The dummy needs to be an array, however.)
!

program test
  implicit none
  call sub(["ab", "cd"])
contains
  subroutine sub(a)
   character(len=4) :: a(1)
   print *, a(1)
  end subroutine sub
end program test
