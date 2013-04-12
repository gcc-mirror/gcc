! { dg-do compile }
! { dg-options "-std=f95" }
! { dg-shouldfail "Fortran 2003 feature with -std=f95" }
! Test whether import does not work with -std=f95
! PR fortran/29601

module testmod
  implicit none
  integer, parameter :: kind = 8
  type modType
    real :: rv
  end type modType
  interface
    subroutine other(x,y)
      import ! { dg-error "Fortran 2003: IMPORT statement" }
      type(modType) :: y ! { dg-error "is being used before it is defined" }
      real(kind)    :: x ! { dg-error "has not been declared" }
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
      import ! { dg-error "Fortran 2003: IMPORT statement" }
      type(myType) :: x ! { dg-error "is being used before it is defined" }
      integer(dp)  :: y ! { dg-error "has not been declared" }
    end subroutine bar
    subroutine test(x)
      import :: myType3 ! { dg-error "Fortran 2003: IMPORT statement" }
      import myType3 ! { dg-error "Fortran 2003: IMPORT statement" }
      type(myType3) :: x ! { dg-error "is being used before it is defined" }
    end subroutine test
  end interface

end program foo
