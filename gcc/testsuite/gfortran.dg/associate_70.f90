! { dg-do run }
! ( dg-options "-Wuninitialized" )
!
! Test fix for PR115700 comment 5, in which ‘.tmp1’ is used uninitialized and
! both normal and scalarized array references did not work correctly.
!
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!
  character(4), dimension(3) :: chr = ['abcd', 'efgh', 'ijkl']
  call mvce (chr)
  if (any (chr /= ['ABcd', 'EFgh', 'IJkl'])) stop 1
contains
  subroutine mvce(x)
    implicit none
    character(len=*), dimension(:), intent(inOUT), target :: x
    integer :: i
    i = len(x)

! This was broken
    associate (tmp1 => x(:)(1:i/2))
      if (len (tmp1) /= i/2) stop 2
      if (tmp1(2) /= 'ef') stop 3
      if (any (tmp1 /= ['ab', 'ef', 'ij'])) stop 4
      tmp1 = ['AB','EF','IJ']
    end associate

! Retest things that worked previously.
    associate (tmp2 => x(:)(1:2))
      if (len (tmp2) /= i/2) stop 5
      if (tmp2(2) /= 'EF') stop 6
      if (any (tmp2 /= ['AB','EF','IJ'])) stop 7
    end associate

    associate (tmp3 => x(3)(1:i/2))
      if (len (tmp3) /= i/2) stop 8
      if (tmp3 /= 'IJ') stop 9
    end associate

  end subroutine mvce
end
