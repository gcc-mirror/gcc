! { dg-do compile }
! { dg-options "-Werror -Wunused -std=f95" }
! PR fortran/21061
! gfortran ignores -Werror
! free-form tests

! gfc_notify_std:
       function char_ (ch) ! { dg-warning "Obsolescent feature" }
       character(*) :: char_, ch
        char_ = ch
       end function char_

! warning(0,...):
!      function wrong_warn (i) ! { -warning "Function does not return a value" }
!      integer i
!      end function wrong_warn

       implicit none
! gfc_warning:
1234  complex :: cplx ! { dg-warning "defined but cannot be used" }
      cplx = 20.

! gfc_warning_now:
 1 ! { dg-warning "Ignoring statement label in empty statement" }
       end
! { dg-final { output-exists-not } }
