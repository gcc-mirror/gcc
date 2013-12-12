! { dg-do compile { target { ! *-*-* } } }
!
! To be compiled with "-g" via namelist_83.f90
!
! PR fortran/59440
!
! Contributed by Harald Anlauf
!
! Was ICEing during DWARF generation.
!
! This is the second file, the module is in namelist_83.f90
!

!
MODULE gfcbug126
  use mo_t_datum, only: qbit_conv
  implicit none
  namelist /OBSERVATIONS/ qbit_conv
end module gfcbug126

! As we have to link, add an empty main program:
end
