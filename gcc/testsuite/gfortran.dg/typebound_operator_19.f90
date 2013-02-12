! { dg-do compile }
!
! PR fortran/54195
! The compiler used to diagnose a duplicate entity in the assignment interface
! because NC was resolved twice.
!
! Contributed by Damian Rouson <damian@rouson.net>

module import_clashes_with_generic

  type ,abstract :: foo
  contains
    procedure :: unary
    generic :: operator(-) => unary
  end type

  abstract interface
    integer function bar()
      import :: foo
    end function
  end interface

contains

  integer function unary(rhs)
    class(foo) ,intent(in) :: rhs
  end function

end module
