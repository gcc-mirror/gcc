! { dg-do compile }
! { dg-options "-Waliasing" }
! PR fortran/91296
! Code contributed by Chinoune Mehdi <chinoune dot medhi at hotmail dot com> 
module m
  implicit none
  integer, parameter :: sp = selected_real_kind(6)

contains
  pure subroutine s(a,b,c)
    real(sp), intent(in) :: a, b
    real(sp), intent(out) :: c
    c = a + b
  end subroutine s
end module m

program test
  use m
  implicit none
  real(sp) :: a
  complex(sp) :: c

  c = (1._sp,1._sp)
  call s(c%re,c%im,a)   ! *** This use to cause an ICE. ***
  print*,a

end program test
