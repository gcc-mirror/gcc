! { dg-do run }
! ( dg-options "-Wuninitialized" )
!
! Test fix for PR115700 comment 5, in which ‘.tmp1’ is used uninitialized and
! both normal and scalarized array references did not work correctly.
! This testcase checks for correct results and associate_69 for suppression
! of the bogus error.
!
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!
  character(4), dimension(3) :: chr = ['abcd', 'efgh', 'ijkl']
  call mvce (chr)
  if (any (chr /= ['ABcd', 'EFgh', 'IJkl'])) stop 1

contains
  subroutine mvce(x)
    implicit none
    character(len=*), dimension(:), intent(inOUT) :: x
    integer :: i
    i = len(x)

    associate (tmp1 => x)
      if (len (tmp1) /= len (x)) stop 2
      tmp1(2)(3:4) = '12'
    end associate
    if (any (x /= ['abcd', 'ef12', 'ijkl'])) stop 3

    associate (tmp2 => x(1:))
      if (len (tmp2) /= len (x)) stop 4
      tmp2(2)(1:2) = '34'
    end associate
    if (any (x /= ['abcd', '3412', 'ijkl'])) stop 5

    associate (tmp3 => x(1:)(:))
      if (len (tmp3) /= len (x)) stop 6
      tmp3(3)(3:4) = '56'
    end associate
    if (any (x /= ['abcd', '3412', 'ij56'])) stop 7

    associate (tmp4 => x(:)(1:))
      if (len (tmp4) /= len (x)) stop 8
      tmp4(3)(1:2) = '78'
    end associate
    if (any (x /= ['abcd', '3412', '7856'])) stop 9

    associate (tmp5 => x(1:)(1:))
      if (len (tmp5) /= len (x)) stop 10
      tmp5 = ['abcd', 'efgh', 'ijkl']
    end associate
    if (any (x /= ['abcd', 'efgh', 'ijkl'])) stop 11

    associate (tmp6 => x(:)(1:i/2))
      if (len (tmp6) /= i/2) stop 11
      if (tmp6(2) /= 'ef') stop 12
      if (any (tmp6 /= ['ab', 'ef', 'ij'])) stop 13
      tmp6 = ['AB','EF','IJ']
    end associate

  end subroutine mvce
end
