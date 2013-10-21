! { dg-do compile }
!
! PR fortran/58652
!
! Contributed by Vladimir Fuka
!
! The passing of a CLASS(*) to a CLASS(*) was reject before
!
module gen_lists
  type list_node
    class(*),allocatable :: item
    contains
      procedure :: move_alloc => list_move_alloc
  end type

  contains

    subroutine list_move_alloc(self,item)
      class(list_node),intent(inout) :: self
      class(*),intent(inout),allocatable :: item

      call move_alloc(item, self%item)
    end subroutine
end module

module lists
  use gen_lists, only: node => list_node
end module lists


module sexp
  use lists
contains
 subroutine parse(ast)
    class(*), allocatable, intent(out) :: ast
    class(*), allocatable :: expr
    integer :: ierr
    allocate(node::ast)
    select type (ast)
      type is (node)
        call ast%move_alloc(expr)
    end select
  end subroutine
end module
