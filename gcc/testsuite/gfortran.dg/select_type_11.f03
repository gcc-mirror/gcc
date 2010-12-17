! { dg-do compile }
!
! PR 42335: [OOP] ICE on CLASS IS (bad_identifier)
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

  implicit none
  type, abstract :: vector_class
  end type vector_class

  type, extends(vector_class) :: trivial_vector_type
    real :: elements(100)
  end type trivial_vector_type

contains

  subroutine bar (this,v)
    class(trivial_vector_type), intent(inout) :: this
    class(vector_class),        intent(in)    :: v

    select type (v)
    class is (bad_id)                    ! { dg-error " error in CLASS IS specification" }
       this%elements(:) = v%elements(:)  ! { dg-error "is not a member of" }
    end select

  end subroutine bar

end
