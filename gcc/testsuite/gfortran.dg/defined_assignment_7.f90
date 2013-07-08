! { dg-do compile }
!
! PR fortran/57508
!
module ForTrilinos_ref_counter
  type ref_counter
  contains
      procedure :: assign
      generic   :: assignment(=) => assign
  end type
contains
  subroutine assign (lhs, rhs)
    class (ref_counter), intent(inout) :: lhs
    class (ref_counter), intent(in) :: rhs
  end subroutine
end module
module FEpetra_BlockMap
  use ForTrilinos_ref_counter, only : ref_counter
  type :: Epetra_BlockMap 
    type(ref_counter) :: counter
  end type
contains
  function from_struct() result(new_Epetra_BlockMap)
    type(Epetra_BlockMap) :: new_Epetra_BlockMap
  end function
  type(Epetra_BlockMap) function create_arbitrary()
    create_arbitrary = from_struct()
  end function
end module
