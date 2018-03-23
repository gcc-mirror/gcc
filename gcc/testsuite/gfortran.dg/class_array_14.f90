! { dg-do run }
!
! PR fortran/54618
!
! Check whether default initialization works with INTENT(OUT)
! and ALLOCATABLE and no segfault occurs with OPTIONAL.
!

subroutine test1()
  type typ1
    integer :: i = 6
  end type typ1

  type(typ1) :: x

  x%i = 77
  call f(x)
  if (x%i /= 6) STOP 1
  call f()
contains
  subroutine f(y1)
    class(typ1), intent(out), optional :: y1
  end subroutine f
end subroutine test1

subroutine test2()
  type mytype
  end type mytype
  type, extends(mytype):: mytype2
  end type mytype2

  class(mytype), allocatable :: x,y
  allocate (mytype2 :: x)
  call g(x)
  if (allocated (x) .or. .not. same_type_as (x,y)) STOP 2

  allocate (mytype2 :: x)
  call h(x)
  if (allocated (x) .or. .not. same_type_as (x,y)) STOP 3

  call h()
contains
  subroutine g(y2)
    class(mytype), intent(out), allocatable :: y2
  end subroutine g
  subroutine h(y3)
    class(mytype), optional, intent(out), allocatable :: y3
  end subroutine h
end subroutine test2

call test1()
call test2()
end
