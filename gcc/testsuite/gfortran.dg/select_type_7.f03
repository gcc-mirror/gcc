! { dg-do run }
!
! PR 41766: [OOP] SELECT TYPE selector as actual argument with INTENT(INOUT)
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 implicit none

 type t1
   integer :: a
 end type

 type, extends(t1) :: t2
   integer :: b
 end type

 class(t1),allocatable :: cp

 allocate(t2 :: cp)

 select type (cp)
   type is (t2)
     cp%a = 98
     cp%b = 76
     call s(cp)
     print *,cp%a,cp%b
     if (cp%a /= cp%b) call abort()
   class default
     call abort()
 end select

contains

  subroutine s(f)
    type(t2), intent(inout) :: f
    f%a = 3
    f%b = 3
  end subroutine

end
