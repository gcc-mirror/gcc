! { dg-do compile }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m
   implicit none
   type t
      procedure(f), pointer, nopass :: g
   end type
contains
   function f()
      character(:), allocatable :: f
      f = 'abc'
   end
   subroutine s
      type(t) :: z
      z%g = 'x'  ! { dg-error "is a procedure pointer" }
      if ( z%g() /= 'abc' ) stop
   end
end
