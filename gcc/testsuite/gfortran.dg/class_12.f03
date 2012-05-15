! { dg-do compile }
!
! PR 41556: [OOP] Errors in applying operator/assignment to an abstract type
!
! Contributed by Damian Rouson <damian@rouson.net>

module abstract_algebra
  implicit none 
  private      
  public :: rescale
  public :: object

  type ,abstract :: object
  contains
    procedure(assign_interface) ,deferred :: assign   
    procedure(product_interface) ,deferred :: product
    generic  :: assignment(=) => assign
    generic  :: operator(*) => product
  end type 

  abstract interface
    function product_interface(lhs,rhs) result(product)
      import :: object
      class(object) ,intent(in)  :: lhs
      class(object) ,allocatable :: product
      real          ,intent(in)  :: rhs
    end function 
    subroutine assign_interface(lhs,rhs) 
      import :: object 
      class(object) ,intent(inout) :: lhs
      class(object) ,intent(in)    :: rhs
    end subroutine 
  end interface

contains

  subroutine rescale(operand,scale)    
    class(object)    :: operand
    real ,intent(in) :: scale
    operand = operand*scale
    operand = operand%product(scale)
  end subroutine 
end module
