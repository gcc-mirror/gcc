! { dg-do run }
! Test whether import works
! PR fortran/29601

subroutine test(x)
  type myType3
    sequence
    integer :: i
  end type myType3
  type(myType3) :: x
  if(x%i /= 7) call abort()
  x%i = 1
end subroutine test


subroutine bar(x,y)
  type myType
    sequence
    integer :: i
  end type myType
  type(myType) :: x
  integer(8) :: y
  if(y /= 8) call abort()
  if(x%i /= 2) call abort()
  x%i = 5
  y = 42
end subroutine bar

module testmod
  implicit none
  integer, parameter :: kind = 8
  type modType
    real :: rv
  end type modType
  interface
    subroutine other(x,y)
       import
       real(kind)    :: x
       type(modType) :: y
    end subroutine
  end interface
end module testmod

program foo
  integer, parameter :: dp = 8
  type myType
    sequence
    integer :: i
  end type myType
  type myType3
    sequence
    integer :: i
  end type myType3
  interface
    subroutine bar(x,y)
      import
      type(myType) :: x
      integer(dp)     :: y
    end subroutine bar
    subroutine test(x)
      import :: myType3
      import myType3 ! { dg-warning "already IMPORTed from" }
      type(myType3) :: x
    end subroutine test
  end interface

  type(myType) :: y
  type(myType3) :: z
  integer(8) :: i8
  y%i = 2
  i8 = 8
  call bar(y,i8)
  if(y%i /= 5 .or. i8/= 42) call abort()
  z%i = 7
  call test(z)
  if(z%i /= 1) call abort()
end program foo
! { dg-final { cleanup-modules "testmod" } }
