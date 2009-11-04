! { dg-do compile }
!
! PR 41556
! Contributed by Damian Rouson <damian@rouson.net>

  implicit none

  type ,abstract :: object
  contains
    procedure(assign_interface) ,deferred :: assign   
    generic  :: assignment(=) => assign
  end type 

  abstract interface
    subroutine assign_interface(lhs,rhs) 
      import :: object 
      class(object) ,intent(inout) :: lhs
      class(object) ,intent(in)    :: rhs
    end subroutine 
  end interface

! PR 41937
! Contributed by Juergen Reuter <reuter@physik.uni-freiburg.de>

  type, abstract :: cuba_abstract_type
     integer :: dim_f = 1
     real, dimension(:), allocatable :: integral
  end type cuba_abstract_type

contains

    subroutine cuba_abstract_alloc_dim_f(this)
      class(cuba_abstract_type) :: this
      allocate(this%integral(this%dim_f))
    end subroutine cuba_abstract_alloc_dim_f

end
