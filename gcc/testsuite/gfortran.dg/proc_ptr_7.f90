! { dg-do run }
! { dg-additional-sources proc_ptr_7.c }
!
! PR fortran/32580
! Procedure pointer test
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program proc_pointer_test
  use iso_c_binding, only: c_int
  implicit none

  interface
    subroutine assignF(f)
      import c_int
      procedure(Integer(c_int)), pointer :: f
    end subroutine
  end interface

  procedure(Integer(c_int)), pointer :: ptr

  call assignF(ptr)
  if(ptr() /= 42) call abort()

  ptr => f55
  if(ptr() /= 55) call abort()  

  call foo(ptr)
  if(ptr() /= 65) call abort()  

contains

 subroutine foo(a)
   procedure(integer(c_int)), pointer :: a
   if(a() /= 55) call abort()
   a => f65
   if(a() /= 65) call abort()
 end subroutine foo

 integer(c_int) function f55()
    f55 = 55
 end function f55

 integer(c_int) function f65()
    f65 = 65
 end function f65
end program proc_pointer_test
