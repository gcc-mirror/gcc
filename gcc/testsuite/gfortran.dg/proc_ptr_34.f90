! { dg-do compile }
!
! PR fortran/52469
!
! This was failing as the DECL of the proc pointer "func"
! was used for the interface of the proc-pointer component "my_f_ptr"
! rather than the decl of the proc-pointer target
!
! Contributed by palott@gmail.com
!

module ExampleFuncs
  implicit none

  ! NOTE: "func" is a procedure pointer!
  pointer :: func
  interface
     function func (z)
        real :: func
        real, intent (in) :: z
     end function func
  end interface

  type Contains_f_ptr
     procedure (func), pointer, nopass :: my_f_ptr
  end type Contains_f_ptr
contains

function f1 (x)
  real :: f1
  real, intent (in) :: x

  f1 = 2.0 * x

  return
end function f1

function f2 (x)
   real :: f2
   real, intent (in) :: x

   f2 = 3.0 * x**2

   return
end function f2

function fancy (func, x)
   real :: fancy
   real, intent (in) :: x

   interface AFunc
      function func (y)
         real :: func
         real, intent (in) ::y
      end function func
   end interface AFunc

   fancy = func (x) + 3.3 * x
end function fancy

end module  ExampleFuncs


program test_proc_ptr
  use ExampleFuncs
  implicit none

  type (Contains_f_ptr), dimension (2) :: NewType
 
  !NewType(1) % my_f_ptr => f1
  NewType(2) % my_f_ptr => f2

  !write (*, *) NewType(1) % my_f_ptr (3.0), NewType(2) % my_f_ptr (3.0)
  write (6, *)  NewType(2) % my_f_ptr (3.0) ! < Shall print '27.0'

  stop
end program test_proc_ptr

! { dg-final { cleanup-modules "examplefuncs" } }
