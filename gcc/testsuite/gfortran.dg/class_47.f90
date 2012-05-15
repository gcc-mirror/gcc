! { dg-do compile }
!
! PR fortran/51913
!
! Contributed by Alexander Tismer
!
MODULE m_sparseMatrix

  implicit none
  
  type :: sparseMatrix_t

  end type sparseMatrix_t
END MODULE m_sparseMatrix

!===============================================================================
module m_subroutine
!  USE m_sparseMatrix !< when uncommenting this line program works fine

  implicit none

  contains
  subroutine test(matrix)
    use m_sparseMatrix
    class(sparseMatrix_t), pointer :: matrix
  end subroutine
end module

!===============================================================================
PROGRAM main
  use m_subroutine
  USE m_sparseMatrix
  implicit none

  CLASS(sparseMatrix_t), pointer :: sparseMatrix
  
  call test(sparseMatrix)
END PROGRAM
