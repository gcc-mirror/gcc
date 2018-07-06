! { dg-do run }
!
! PR 44541: [OOP] wrong code for polymorphic variable with INTENT(OUT)/Alloc w/ MOLD
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

  implicit none

  type t
    integer :: a = 1
  end type t

  type, extends(t) :: t2
    integer :: b = 3
  end type t2

  type(t2) :: y

  y%a = 44
  y%b = 55
  call intent_out (y)
  if (y%a/=1 .or. y%b/=3) STOP 1

  y%a = 66
  y%b = 77
  call intent_out_unused (y)
  if (y%a/=1 .or. y%b/=3) STOP 2

contains

  subroutine intent_out(x)
    class(t), intent(out) :: x
    select type (x)
      type is (t2)
      if (x%a/=1 .or. x%b/=3) STOP 3
    end select
  end subroutine

   subroutine intent_out_unused(x)
     class(t), intent(out) :: x
   end subroutine

end
