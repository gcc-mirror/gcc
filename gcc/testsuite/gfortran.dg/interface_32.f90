! { dg-do compile }
module m1
  implicit none

  type, abstract :: vector_class
  end type vector_class
end module m1
!---------------------------------------------------------------
module m2
  use m1
  implicit none

  type, abstract :: inner_product_class
  contains
    procedure(dot), deferred :: dot_v_v
    procedure(dot), deferred :: dot_g_g
    procedure(sub), deferred :: D_times_v
    procedure(sub), deferred :: D_times_g
  end type inner_product_class

  abstract interface
    function dot (this,a,b)
      import :: inner_product_class
      import :: vector_class
      class(inner_product_class), intent(in) :: this
      class(vector_class),        intent(in) :: a,b
      real                                   :: dot
    end function
    subroutine sub (this,a)
      import :: inner_product_class
      import :: vector_class
      class(inner_product_class), intent(in)    :: this
      class(vector_class),        intent(inout) :: a
    end subroutine
  end interface
end module m2
!---------------------------------------------------------------
module m3
  use :: m1
  use :: m2
  implicit none
  private
  public :: gradient_class

  type, abstract, extends(vector_class) :: gradient_class
    class(inner_product_class), pointer :: my_inner_product => NULL()
  contains
    procedure, non_overridable  :: inquire_inner_product
    procedure(op_g_v), deferred :: to_vector
  end type gradient_class

  abstract interface
    subroutine op_g_v(this,v)
      import vector_class
      import gradient_class
      class(gradient_class), intent(in)    :: this
      class(vector_class),   intent(inout) :: v
    end subroutine
  end interface
contains
  function inquire_inner_product (this)
    class(gradient_class)               :: this
    class(inner_product_class), pointer :: inquire_inner_product

    inquire_inner_product => this%my_inner_product
  end function inquire_inner_product
end module m3
!---------------------------------------------------------------
module m4
  use m3
  use m2
  implicit none
contains
  subroutine cg (g_initial)
    class(gradient_class),  intent(in)    :: g_initial

    class(inner_product_class), pointer   :: ip_save
    ip_save => g_initial%inquire_inner_product()
  end subroutine cg
end module m4
! { dg-final { cleanup-modules "m1 m2 m3 m4" } }
