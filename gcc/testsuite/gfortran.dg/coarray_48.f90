! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! Fix for P99818 in which wrong code caused an ICE.
!
! Contributed by Gerhard Steinmetz <gscfq@t-online.de>
!
module m
   type t
      integer :: a
   contains
      procedure :: s
   end type
contains
   subroutine s(x)
      class(t) :: x[*]
   end
end
program p
   use m
   associate (y => t(1))
      call y%s           ! { dg-error "must be a coarray" }
   end associate
end
