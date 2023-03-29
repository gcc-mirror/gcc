! { dg-do run }
!
! Test assumed rank finalizers
!
module finalizable_m
! F2018: 7.5.6.2 para 1: "Otherwise, if there is an elemental final
! subroutine whose dummy argument has the same kind type parameters
! as the entity being finalized, or a final subroutine whose dummy
! argument is assumed-rank with the same kind type parameters as the
! entity being finalized, it is called with the entity as an actual
! argument."
  implicit none

  type finalizable_t
    integer :: component_
  contains
    final :: finalize
  end Type

  interface finalizable_type
    module procedure construct0, construct1
  end interface

  integer :: final_ctr = 0

contains

  pure function construct0(component) result(finalizable)
    integer, intent(in) :: component
    type(finalizable_t) finalizable
    finalizable%component_ = component
  end function

  impure function construct1(component) result(finalizable)
    integer, intent(in), dimension(:) :: component
    type(finalizable_t), dimension(:), allocatable :: finalizable
    integer :: sz
    sz = size(component)
    allocate (finalizable (sz))
    finalizable%component_ = component
  end function

  subroutine finalize(self)
    type(finalizable_t), intent(inout), dimension (..) :: self
    select rank (self)
    rank (0)
        print *, "rank 0 value = ", self%component_
    rank (1)
        print *, "rank 1 value = ", self%component_
    rank default
        print *, "rank default"
    end select
    final_ctr = final_ctr + 1
  end subroutine

end module

program specification_expression_finalization
  use finalizable_m
  implicit none

  type(finalizable_t) :: a = finalizable_t (1)
  type(finalizable_t) :: b(2) = [finalizable_t (2), finalizable_t (3)]

  a = finalizable_type (42)
  if (final_ctr .ne. 2) stop 1
  b = finalizable_type ([42, 43])
  print *, b%component_

end program
