! { dg-do compile }
! PR fortran/82796
! Code contributed by ripero84 at gmail dot com 
module eq
   implicit none
   integer :: n1, n2
   integer, dimension(2) :: a
   equivalence (a(1), n1)
   equivalence (a(2), n2)
   common /a/ a
end module eq

module m
   use eq
   implicit none
   type, public :: t
     integer :: i
   end type t
end module m

module p
   implicit none
   contains
   pure integer function d(h)
     use m
     implicit none
     integer, intent(in) :: h
     d = h
   end function
end module p

module q
   implicit none
   contains
   pure integer function d(h)
     use m, only : t
     implicit none
     integer, intent(in) :: h
     d = h
   end function
end module q

module r
   implicit none
   contains
   pure integer function d(h)
     use m, only : a          ! { dg-error "cannot be an EQUIVALENCE object" }
     implicit none
     integer, intent(in) :: h
     d = h
   end function
end module r
