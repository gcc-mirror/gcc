! { dg-do compile }
! { dg-options "-std=f95" }
!
! Test the implementation of inquiry part references (PR40196):
! Check the standards are correctly adhered to.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
program main
   character(4) :: a
   complex :: z
   integer :: i
   a%len = 2       ! { dg-error "Fortran 2003: LEN part_ref" }
   i = a%kind      ! { dg-error "Fortran 2003: KIND part_ref" }
   print *, z%re   ! { dg-error "Fortran 2008: RE or IM part_ref" }
   print *, z%im   ! { dg-error "Fortran 2008: RE or IM part_ref" }
end
