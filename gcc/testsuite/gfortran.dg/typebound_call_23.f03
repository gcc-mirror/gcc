! { dg-do compile }
!
! PR 52968: [OOP] Call to type-bound procedure wrongly rejected
!
! Contributed by Reuben Budiardja <reubendb@gmail.com>

module SolverModule

 type :: SolverType
   class ( EquationTemplate ), pointer :: Equation
 end type

 type :: EquationTemplate
 contains
   procedure, nopass :: Evaluate
 end type

contains

  subroutine Evaluate ()
  end subroutine

 subroutine Solve
   type ( SolverType ) :: S
   call S % Equation % Evaluate ()
 end subroutine

end module
