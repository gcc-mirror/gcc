! { dg-do compile }
! Tests the fix for PR37597, where the reference to other_sub would generate
! Error: Symbol 'other_sub' at (1) has no IMPLICIT type.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
! from a report on clf by Rich Townsend <rhdt@barvoidtol.udel.edu>
!
module foo
   implicit none
contains
   subroutine main_sub ()
     call internal_sub()
   contains
     subroutine internal_sub()
       call QAG(other_sub)
     end subroutine internal_sub
   end subroutine main_sub
   subroutine other_sub ()
   end subroutine other_sub
end module foo
! { dg-final { cleanup-modules "foo" } }
