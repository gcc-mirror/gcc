! { dg-do compile }
! Tests the fix for PR29976, in which the call to CMPLX caused an
! ICE with an optional dummy for the imaginary part.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
SUBROUTINE pw_sumup (alpha_im)
  REAL, INTENT(in), OPTIONAL :: alpha_im
  COMPLEX :: my_alpha_c
  IF (PRESENT(alpha_im)) THEN
     my_alpha_c = CMPLX(0.,alpha_im)
  END IF
END SUBROUTINE pw_sumup

! Check non-intrinsic functions.
SUBROUTINE pw_sumup_2 (alpha_im)
  REAL, INTENT(in), OPTIONAL :: alpha_im
  COMPLEX :: my_alpha_c
  IF (PRESENT(alpha_im)) THEN
     my_alpha_c = MY_CMPLX(0.,alpha_im)
  END IF
contains
  complex function MY_CMPLX (re, im)
    real, intent(in) :: re
    real, intent(in), optional :: im
    if (present (im)) then 
      MY_CMPLX = cmplx (re, im)
    else
      MY_CMPLX = cmplx (re, 0.0)
    end if
  end function MY_CMPLX
END SUBROUTINE pw_sumup_2
