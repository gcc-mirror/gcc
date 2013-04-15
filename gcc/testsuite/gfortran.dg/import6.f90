! { dg-do compile }
! Tests the fix for PR32827, in which IMPORT :: my_type put the
! symbol into the interface namespace, thereby generating an error
! when the declaration of 'x' is compiled.
!
! Contributed by Douglas Wells <sysmaint@contek.com>
!
subroutine func1(param)
  type :: my_type
    sequence
    integer :: data
  end type my_type
  type(my_type) :: param
  param%data = 99
end subroutine func1

subroutine func2(param)
  type :: my_type
    sequence
    integer :: data
  end type my_type
  type(my_type) :: param
  param%data = 21
end subroutine func2

  type :: my_type
    sequence
    integer :: data
  end type my_type

  interface
    subroutine func1(param)
      import :: my_type
      type(my_type) :: param
    end subroutine func1
  end interface
  interface
    subroutine func2(param)
      import
      type(my_type) :: param
    end subroutine func2
  end interface

  type(my_type) :: x
  call func1(x)
  print *, x%data
  call func2(x)
  print *, x%data
end
