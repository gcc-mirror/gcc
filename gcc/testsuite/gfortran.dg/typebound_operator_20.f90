! { dg-do run }
!
! PR 63733: [4.8/4.9/5 Regression] [OOP] wrong resolution for OPERATOR generics
!
! Original test case from Alberto F. Martín Huertas <amartin@cimne.upc.edu>
! Slightly modified by Salvatore Filippone <sfilippone@uniroma2.it>
! Further modified by Janus Weil <janus@gcc.gnu.org>

module overwrite
  type parent
   contains
     procedure :: sum => sum_parent
     generic   :: operator(+) => sum
  end type

  type, extends(parent) ::  child
  contains
    procedure :: sum => sum_child
  end type

contains

  integer function sum_parent(op1,op2)
    implicit none
    class(parent), intent(in) :: op1, op2
    sum_parent = 0
  end function

  integer function sum_child(op1,op2)
    implicit none
    class(child) , intent(in) :: op1
    class(parent), intent(in) :: op2
    sum_child = 1
  end function

end module

program drive
  use overwrite
  implicit none

  type(parent) :: m1, m2
  class(parent), pointer :: mres
  type(child)  :: h1, h2
  class(parent), pointer :: hres

  if (m1 + m2 /= 0) call abort()
  if (h1 + m2 /= 1) call abort()
  if (h1%sum(h2) /= 1) call abort()

end

! { dg-final { cleanup-modules "overwrite" } }
