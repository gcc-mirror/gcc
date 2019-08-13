! { dg-do compile }
! PR fortran/88072
! Original code contributed by Andrew Wood <andrew at fluidgravity dot co.uk>
module m1

   implicit none

   type, abstract, public :: t1
      integer, dimension(:), allocatable :: i
      contains
         procedure(f1), deferred :: f
   end type t1

   type, extends(t1), public :: t2 ! { dg-error "must be ABSTRACT because" }
      contains
         procedure :: f => f2    ! { dg-error "mismatch for the overriding" }
   end type t2

   abstract interface
      function f1(this)          ! { dg-error "must be dummy, allocatable or" }
         import
         class(t1) :: this
         class(t1) :: f1
      end function f1
   end interface
   contains
      type(t2) function f2(this)
         class(t2) :: this
      end function f2
end module m1
