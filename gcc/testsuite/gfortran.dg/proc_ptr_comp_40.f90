! { dg-do compile }
!
! PR 64173: [F03] ICE involving procedure pointer component
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

  implicit none

  type :: r_magnus_ivp_t
     integer, allocatable :: jc
     procedure(abscissa_), nopass, pointer :: abscissa_p
  end type

  abstract interface
     function abscissa_ () result (x)
       real, allocatable :: x(:)
     end function
  end interface

contains

 function doinit () result (iv)
   type(r_magnus_ivp_t) :: iv
 end function

end
