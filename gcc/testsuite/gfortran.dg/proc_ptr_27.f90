! { dg-do compile }
!
! PR fortran/44446
!
! Contributed by Marco Restelli.
!
! Procedure pointer with PROTECTED was wrongly rejected.
!
module m
 implicit none
 abstract interface
  pure function i_f(x) result(y)
   real, intent(in) :: x
   real :: y
  end function i_f
 end interface
 procedure(i_f), pointer, protected :: p_f => null()
end module m
