! { dg-do run }
! { dg-options "-O0" }
!
! Test fix for PR18022.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
program assign_func_dtcomp
 implicit none
 type                         ::  mytype
   real                       ::  x
   real                       ::  y
 end type mytype
 type (mytype), dimension (4) ::  z

 type                         ::  thytype
   real                       ::  x(4)
 end type thytype
 type (thytype)               ::  w
 real, dimension (4)          ::  a = (/1.,2.,3.,4./)
 real, dimension (4)          ::  b = (/5.,6.,7.,8./)


! Test the original problem is fixed.
 z(:)%x = foo (a)
 z(:)%y = foo (b)


 if (any(z%x.ne.a).or.any(z%y.ne.b)) STOP 1

! Make sure we did not break anything on the way.
 w%x(:) = foo (b)
 a = foo (b)

 if (any(w%x.ne.b).or.any(a.ne.b)) STOP 2

contains

 function foo (v) result (ans)
   real, dimension (:), intent(in)   ::  v
   real, dimension (size(v))  ::  ans
   ans = v
 end function foo


end program assign_func_dtcomp

