! { dg-do run }
!
! Test the fix for PR88393 in which a segfault occurred as indicated.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
module m
   implicit none
   type :: t
      character(len=:), allocatable :: cs
   contains
      procedure :: ass
      generic :: assignment(=) => ass
   end type
contains
   subroutine ass(a, b)
      class(t), intent(inout) :: a
      class(t), intent(in)    :: b
      a%cs = b%cs
      print *, "ass"
   end subroutine
end module

program p
   use m
   implicit none
   type :: t2
      type(t) :: c
   end type
   type(t2), dimension(1:2) :: arr
   arr(1)%c%cs = "abcd"
   arr(2)%c = arr(1)%c  ! Segfault here.
   print *, "done", arr(2)%c%cs, arr(2)%c%cs
! Make sure with valgrind that there are no memory leaks.
   deallocate (arr(1)%c%cs)
   deallocate (arr(2)%c%cs)
end
