! { dg-do run }
!
! Test the fix for PR84674, in which the non-overridable variant of the
! procedure ff below caused a runtime segfault.
!
! Contributed by Jakub Benda  <albandil@atlas.cz>
!
 module m
  implicit none

  type, abstract :: t1
   integer :: i
  contains
   procedure(i_f), pass(u), deferred :: ff
  end type t1

  type, abstract, extends(t1) :: t2
  contains
   procedure, non_overridable, pass(u) :: ff => f ! Segmentation fault
   !procedure, pass(u) :: ff => f ! worked
  end type t2

  type, extends(t2) :: DerivedType
  end type DerivedType

  abstract interface
   subroutine i_f(u)
    import :: t1
    class(t1), intent(inout) :: u
   end subroutine i_f
  end interface

 contains

  subroutine f(u)
   class(t2), intent(inout) :: u
    u%i = 3*u%i
  end subroutine f

 end module m


 program p

  use m

  implicit none

  class(t1), allocatable :: v

  allocate(DerivedType::v)
  v%i = 2
  call v%ff()
  if (v%i /= 6) stop
 end program p
