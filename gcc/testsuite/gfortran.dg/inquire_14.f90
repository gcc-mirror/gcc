! { dg-do compile }
! PR47583 Inquire affected by previous read.
subroutine input(indat)
   real indat(:)
   read(*,*) indat
end subroutine input

subroutine abc(sizedat)
   real, intent(in) :: sizedat(:)
   integer :: rl
   inquire(iolength=rl) sizedat
   write(*,*) rl
end subroutine abc
