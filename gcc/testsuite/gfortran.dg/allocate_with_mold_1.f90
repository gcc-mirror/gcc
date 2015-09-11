! { dg-do run }
!
! Fixes a bug that emerged from the fix of PR62044 - see the PR. When
! there was no default initializer, code-expr3 was set null and so the
! vpointer was set to the vtable of the declared type, rather than that
! of the MOLD expression.
!
! Contributed by but based on the original PR62044 testcase by
! Paul Thomas  <pault@gcc.gnu.org>
!
module GridImageSilo_Template
  implicit none
  type, public, abstract :: GridImageSiloTemplate
  end type GridImageSiloTemplate
end module GridImageSilo_Template

module UnstructuredGridImageSilo_Form
  use GridImageSilo_Template
  implicit none
  type, public, extends ( GridImageSiloTemplate ) :: &
    UnstructuredGridImageSiloForm
  end type UnstructuredGridImageSiloForm
end module UnstructuredGridImageSilo_Form

module UnstructuredGridImages
  use UnstructuredGridImageSilo_Form, &
        UnstructuredGridImageForm => UnstructuredGridImageSiloForm
contains
  subroutine foo
    class (GridImageSiloTemplate), allocatable :: a
    type (UnstructuredGridImageForm) :: b
    integer :: i = 0
    allocate (a, mold = b)
    select type (a)
      type is (UnstructuredGridImageForm)
        i = 1
      class default
        i = 2
    end select
    if (i .ne. 1) call abort
  end subroutine
end module UnstructuredGridImages

  use UnstructuredGridImages
  call foo
end

