! { dg-do compile }
!
! Test part of the fix for PR99124 which adds errors for class results
! That violate F2018, C15100.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m
   type t
      integer :: i
   contains
      procedure :: f
      generic :: operator(+) => f
   end type
contains
   elemental function f(a, b) &
   result(c)                     ! { dg-error "shall not have an ALLOCATABLE or POINTER attribute" }
      class(t), intent(in) :: a, b
      class(t), allocatable :: c
      c = t(a%i + b%i)
   end
   elemental function g(a, b) &
   result(c)                     ! { dg-error "shall not have an ALLOCATABLE or POINTER attribute" }
      class(t), intent(in) :: a, b
      class(t), pointer :: c
      c => null ()
   end
   elemental function h(a, b) &  ! { dg-error "must have a scalar result" }
   result(c)                     ! { dg-error "must be dummy, allocatable or pointer" }
      class(t), intent(in) :: a, b
      class(t) :: c(2)
   end
end
