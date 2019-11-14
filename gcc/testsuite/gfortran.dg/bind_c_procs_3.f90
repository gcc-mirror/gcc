! { dg-do run }
!
! Test the fix for PR92123, in which 'dat' caused an error with the message
! "Scalar variable 'dat' at ?? with POINTER or ALLOCATABLE in procedure Fsub
! with BIND(C) is not yet supported."
!
! Contributed by Vipul Parekh  <parekhvs@gmail.com>
!
module m
   use, intrinsic :: iso_c_binding, only : c_int
contains
   subroutine Fsub( dat ) bind(C, name="Fsub")
      !.. Argument list
      integer(c_int), allocatable, intent(out) :: dat
      dat = 42
      return
   end subroutine
end module m

   use, intrinsic :: iso_c_binding, only : c_int
   use m, only : Fsub
   integer(c_int), allocatable :: x
   call Fsub( x )
   if (x .ne. 42) stop 1
end
