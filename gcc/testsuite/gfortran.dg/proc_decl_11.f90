! { dg-do compile }
! PR fortran/33917
!
! Depending, in which order the symbol tree
! was walked in resolve, gfortran resolved
! p6 before p4; thus there was no explicit
! interface available for p4 and an error
! was printed. (This is a variant of proc_decl_2.f90)
!
! Additionally, the following contrain was not honoured:
! "C1212 (R1215) [...] If name is declared by a procedure-declaration-stmt
! it shall be previously declared." ("name" = interface-name)
!
program s
  implicit none
  procedure() :: q2
  procedure() :: q3
  procedure() :: q5
  procedure(sub) :: p4
  procedure(p4) :: p6
contains
  subroutine sub
  end subroutine
end program s

subroutine test
  implicit none
  abstract interface
    subroutine sub()
    end subroutine sub
  end interface
  procedure(p4) :: p6 ! { dg-error "declared in a later PROCEDURE statement" }
  procedure(sub) :: p4
end subroutine test
