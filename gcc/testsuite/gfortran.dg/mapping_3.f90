! { dg-do run }
! Tests the fix for PR33888, in which the character length of
! the elemental function myfunc was not being calculated before
! the temporary for the array result was allocated.
!
! Contributed by Walter Spector <w6ws@earthlink.net>
!
program ftn95bug
  implicit none

  character(8) :: indata(4) =  &
              (/ '12344321', '98766789', 'abcdefgh', 'ABCDEFGH' /)

  call process (myfunc (indata))  ! <- This caused a gfortran ICE !

contains

  elemental function myfunc (s)
    character(*), intent(in) :: s
    character(len (s)) :: myfunc

    myfunc = s

  end function

  subroutine process (strings)
    character(*), intent(in) :: strings(:)

    if (any (strings .ne. indata)) call abort ()

  end subroutine

end program
