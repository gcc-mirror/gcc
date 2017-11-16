! { dg-do compile }
!
! Code contributed by Gerhard Steinmetz
!
program pr82620
   type t(a)
      integer, len :: a
   end type
   type(t(:)), allocatable :: x, y
   allocate(t(4) :: x)
   allocate)t(7) :: y)     ! { dg-error "Syntax error in ALLOCATE" }
end program pr82620
