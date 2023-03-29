! { dg-do compile }
! { dg-additional-options "-fcheck=bounds -Wno-analyzer-malloc-leak" }
! Copy of gfortran.dg/bounds_check_23.f90
! as a regression test for ICE with -fanalyzer (PR analyzer/108065)

program test
  implicit none
  call sub('Lorem ipsum')
contains
  subroutine sub( text )
    character(len=*), intent(in)  :: text
    character(len=1), allocatable :: c(:)
    integer :: i
    c = [ ( text(i:i), i = 1, len(text) ) ]
    if (c(1) /= 'L') stop 1
  end subroutine sub
end program test
