! { dg-do compile }
! PR fortran/45848
! PR fortran/47204
!
! Contributed by Harald Anlauf and Zdenek Sojka
!
module gfcbug111
  implicit none

  type, abstract :: inner_product_class
  end type inner_product_class

  type, extends(inner_product_class) :: trivial_inner_product_type
  end type trivial_inner_product_type

contains

  function my_dot_v_v (this,a,b)       ! { dg-error "has no IMPLICIT type" }
    class(trivial_inner_product_type), intent(in) :: this
    class(vector_class),               intent(in) :: a,b ! { dg-error "Derived type" }
    real :: my_dot_v_v

    select type (a)                    ! { dg-error "Selector shall be polymorphic" }
    class is (trivial_vector_type)     ! { dg-error "Syntax error in CLASS IS" }
       select type (b)                 ! { dg-error "Expected TYPE IS" }
       class is (trivial_vector_type)  ! { dg-error "Syntax error in CLASS IS" }
       class default
       end select
    class default 
    end select ! { dg-error "Expecting END FUNCTION" }
  end function my_dot_v_v
end module gfcbug111

select type (a)
! { dg-prune-output "Unexpected end of file" }
