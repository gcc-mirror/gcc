! { dg-do compile }
!
! Tests comparisons of MODULE PROCEDURE characteristics and
! the characteristics of their dummies. Also tests the error
! arising from redefining dummies and results in MODULE
! procedures.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
 module foo_interface
   implicit none
   type foo
     character(len=16) :: greeting = "Hello, world!   "
     character(len=16), private :: byebye = "adieu, world!   "
   end type foo

   interface
     module function array1(this) result (that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), allocatable, dimension(:) :: that
     end function
     character(16) module function array2(this, that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), allocatable, dimension(:) :: that
     end function
     module subroutine array3(this, that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), intent(inOUT), allocatable, dimension(:) :: that
     end subroutine
     module subroutine array4(this, that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), intent(inOUT), allocatable, dimension(:) :: that
     end subroutine
     integer module function scalar1 (arg)
        real, intent(in) :: arg
     end function
     module function scalar2 (arg) result(res)
        real, intent(in) :: arg
        real :: res
     end function
      module function scalar3 (arg) result(res)
        real, intent(in) :: arg
        real :: res
     end function
      module function scalar4 (arg) result(res)
        real, intent(in) :: arg
        complex :: res
     end function
      module function scalar5 (arg) result(res)
        real, intent(in) :: arg
        real, allocatable :: res
     end function
      module function scalar6 (arg) result(res)
        real, intent(in) :: arg
        real, allocatable :: res
     end function
      module function scalar7 (arg) result(res)
        real, intent(in) :: arg
        real, allocatable :: res
     end function
   end interface
 end module

!
  SUBMODULE (foo_interface) foo_interface_son
!
  contains

     module function array1 (this) result(that) ! { dg-error "Rank mismatch in function result" }
       type(foo), intent(in), dimension(:) :: this
       type(foo), allocatable :: that
     end function

     character(16) module function array2(this) ! { dg-error "Mismatch in number of MODULE PROCEDURE formal arguments" }
       type(foo), intent(in), dimension(:) :: this
       type(foo), allocatable, dimension(:) :: that
       allocate (that(2), source = this(1))
       that%greeting = that%byebye
       array2 = trim (that(size (that))%greeting(1:5))//", people!"
     end function

     module subroutine array3(thiss, that) ! { dg-error "Mismatch in MODULE PROCEDURE formal argument names" }
       type(foo), intent(in), dimension(:) :: thiss
       type(foo), intent(inOUT), allocatable, dimension(:) :: that
       allocate (that(size(thiss)), source = thiss)
       that%greeting = that%byebye
     end subroutine

     module subroutine array4(this, that, the_other) ! { dg-error "Mismatch in number of MODULE PROCEDURE formal arguments" }
       type(foo), intent(in), dimension(:) :: this
       type(foo), intent(inOUT), allocatable, dimension(:) :: that, the_other
       integer :: i
       allocate (that(size(this)), source = this)
       that%greeting = that%byebye
       do i = 1, size (that)
         that(i)%greeting = trim (that(i)%greeting(1:5))//", people!"
       end do
     end subroutine

     recursive module function scalar1 (arg) ! { dg-error "Mismatch in RECURSIVE" }
        real, intent(in) :: arg
     end function

     pure module function scalar2 (arg) result(res) ! { dg-error "Mismatch in PURE" }
        real, intent(in) :: arg
        real :: res
     end function

     module procedure scalar7
       real, intent(in) :: arg ! { dg-error "redefinition of the declaration" }
       real, allocatable :: res ! { dg-error "redefinition of the declaration" }
     end function ! { dg-error "Expecting END PROCEDURE statement" }
     end procedure ! This prevents a cascade of errors.
  end SUBMODULE foo_interface_son

!
  SUBMODULE (foo_interface) foo_interface_daughter
!
  contains

      module function scalar3 (arg) result(res) ! { dg-error "Type mismatch in argument" }
        integer, intent(in) :: arg
        real :: res
     end function

      module function scalar4 (arg) result(res) ! { dg-error "Type mismatch in function result" }
        real, intent(in) :: arg
        real :: res
     end function

      module function scalar5 (arg) result(res) ! { dg-error "ALLOCATABLE attribute mismatch in function result " }
        real, intent(in) :: arg
        real :: res
     end function

      module function scalar6 (arg) result(res) ! { dg-error "Rank mismatch in argument" }
        real, intent(in), dimension(2) :: arg
        real, allocatable :: res
     end function
  end SUBMODULE foo_interface_daughter
