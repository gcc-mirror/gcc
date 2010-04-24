! { dg-do "compile" }
! Test the fix for PR43266, in which an ICE followed correct error messages.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
! Reported in http://groups.google.ca/group/comp.lang.fortran/browse_thread/thread/f5ec99089ea72b79
!
!----------------
! library code

module m
TYPE, ABSTRACT :: top
CONTAINS
   PROCEDURE(xxx), DEFERRED :: proc_a ! { dg-error "must be a module procedure" }
   ! some useful default behaviour
   PROCEDURE :: proc_c => top_c ! { dg-error "must be a module procedure" }
END TYPE top

! Concrete middle class with useful behaviour
TYPE, EXTENDS(top) :: middle
CONTAINS
   ! do nothing, empty proc just to make middle concrete
   PROCEDURE :: proc_a => dummy_middle_a ! { dg-error "must be a module procedure" }
   ! some useful default behaviour
   PROCEDURE :: proc_b => middle_b ! { dg-error "must be a module procedure" }
END TYPE middle

!----------------
! client code

TYPE, EXTENDS(middle) :: bottom
CONTAINS
   ! useful proc to satisfy deferred procedure in top. Because we've
   ! extended middle we wouldn't get told off if we forgot this.
   PROCEDURE :: proc_a => bottom_a
   ! calls middle%proc_b and then provides extra behaviour
   PROCEDURE :: proc_b => bottom_b
   ! calls top_c and then provides extra behaviour
   PROCEDURE :: proc_c => bottom_c
END TYPE bottom
contains
SUBROUTINE bottom_b(obj)
   CLASS(Bottom) :: obj
   CALL obj%middle%proc_b ! { dg-error "should be a SUBROUTINE" }
   ! other stuff
END SUBROUTINE bottom_b

SUBROUTINE bottom_c(obj)
   CLASS(Bottom) :: obj
   CALL top_c(obj)
   ! other stuff
END SUBROUTINE bottom_c 
end module
! { dg-final { cleanup-modules "m" } }
