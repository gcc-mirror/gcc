! { dg-do compile }
! Test fix for PR37583, in which:
! (i) the reference to glocal prior to the ENTRY caused an internal
! error and
! (ii) the need for a RECURSIVE attribute was ignored.
!
! Contributed by Arjen Markus <arjen.markus@wldelft.nl>
!
module gsub
contains
recursive subroutine suba( g )   ! prefix with "RECURSIVE"
   interface
       real function g(x)
       real x
       end function
   end interface
   real :: x, y
   call mysub( glocala )
   return
entry glocala( x, y )
   y = x
end subroutine
subroutine subb( g )
   interface
       real function g(x)
       real x
       end function
   end interface
   real :: x, y
   call mysub( glocalb ) ! { dg-error "is recursive" }
   return
entry glocalb( x, y )
   y = x
end subroutine
end module
! { dg-final { cleanup-modules "gsub" } }
