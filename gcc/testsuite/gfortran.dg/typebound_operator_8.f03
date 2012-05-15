! { dg-do run }
! PR48946 - complex expressions involving typebound operators of derived types.
!
module field_module
  implicit none
  type ,abstract :: field
  contains
    procedure(field_op_real) ,deferred :: multiply_real
    procedure(field_plus_field) ,deferred :: plus
    procedure(assign_field) ,deferred :: assn
    generic :: operator(*) => multiply_real
    generic :: operator(+) => plus
    generic :: ASSIGNMENT(=) => assn
  end type
  abstract interface
    function field_plus_field(lhs,rhs)
      import :: field
      class(field) ,intent(in)  :: lhs
      class(field) ,intent(in)  :: rhs
      class(field) ,allocatable :: field_plus_field
    end function
  end interface
  abstract interface
    function field_op_real(lhs,rhs)
      import :: field
      class(field) ,intent(in)  :: lhs
      real ,intent(in) :: rhs
      class(field) ,allocatable :: field_op_real
    end function
  end interface
  abstract interface
    subroutine assign_field(lhs,rhs)
      import :: field
      class(field) ,intent(OUT)  :: lhs
      class(field) ,intent(IN)  :: rhs
    end subroutine
  end interface
end module

module i_field_module
  use field_module
  implicit none
  type, extends (field)  :: i_field
    integer :: i
  contains
    procedure :: multiply_real => i_multiply_real
    procedure :: plus => i_plus_i
    procedure :: assn => i_assn
  end type
contains
  function i_plus_i(lhs,rhs)
    class(i_field) ,intent(in)  :: lhs
    class(field) ,intent(in)  :: rhs
    class(field) ,allocatable :: i_plus_i
    integer :: m = 0
    select type (lhs)
      type is (i_field); m = lhs%i
    end select
    select type (rhs)
      type is (i_field); m = rhs%i + m
    end select
    allocate (i_plus_i, source = i_field (m))
  end function
  function i_multiply_real(lhs,rhs)
    class(i_field) ,intent(in)  :: lhs
    real ,intent(in) :: rhs
    class(field) ,allocatable :: i_multiply_real
    integer :: m = 0
    select type (lhs)
      type is (i_field); m = lhs%i * int (rhs)
    end select
    allocate (i_multiply_real, source = i_field (m))
  end function
  subroutine i_assn(lhs,rhs)
    class(i_field) ,intent(OUT)  :: lhs
    class(field) ,intent(IN)  :: rhs
    select type (lhs)
      type is (i_field)
        select type (rhs)
          type is (i_field)
            lhs%i = rhs%i
        end select         
      end select
    end subroutine
end module

program main
  use i_field_module
  implicit none
  type(i_field) ,allocatable :: u
  allocate (u, source = i_field (99))

  u = u*2.
  u = (u*2.0*4.0) + u*4.0
  u = u%multiply_real (2.0)*4.0
  u = i_multiply_real (u, 2.0) * 4.0
  
  if (u%i .ne. 152064) call abort
end program
