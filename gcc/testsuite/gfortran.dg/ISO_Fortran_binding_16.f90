! { dg-do run }
! { dg-additional-options "-fbounds-check" }
! { dg-additional-sources ISO_Fortran_binding_16.c }
!
! Test the fix for PR92142.
!
  use, intrinsic :: iso_c_binding, only: c_int

  implicit none
  
  interface
    function c_setpointer(ip) result(ierr) bind(c)
      use, intrinsic :: iso_c_binding, only: c_int
      type(*), dimension(..), target :: ip
      integer(c_int)                 :: ierr
    end function c_setpointer
  end interface
  
  integer(c_int) :: it = 1
  
  if (c_setpointer(it) /= 0) stop 1
  
end

! { dg-output "CFI_setpointer: Result shall be the address of a C descriptor for a Fortran pointer." }
