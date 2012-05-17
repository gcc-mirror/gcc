! { dg-do compile }
! Test for import in interfaces PR fortran/30922
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module test_import
  implicit none

  type :: my_type
     integer :: data
  end type my_type
  integer, parameter :: n = 20

  interface
     integer function func1(param)
       import
       type(my_type) :: param(n)
     end function func1

     integer function func2(param)
       import :: my_type
       type(my_type), value :: param
     end function func2
  end interface

contains

  subroutine sub1 ()

    interface
      integer function func3(param)
        import
        type(my_type), dimension (n) :: param
      end function func3

      integer function func4(param)
        import :: my_type, n
        type(my_type), dimension (n) :: param
      end function func4
    end interface

  end subroutine sub1
end module test_import
