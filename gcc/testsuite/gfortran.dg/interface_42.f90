! { dg-do compile }
! { dg-options "-fmax-errors=1" }
! PR fortran/84922
! Original code contributed by William Clodius.
module copy

   interface
      module subroutine foo_da(da, copy) ! { dg-error "(1)" }
         integer, intent(in) :: da(:)
         integer, allocatable, intent(out) :: copy(:)
      end subroutine foo_da
   end interface

   contains

      subroutine foo_da(da, copy) ! { dg-error "defined in interface body|PROCEDURE attribute conflicts with PROCEDURE attribute" }
         integer, intent(in) :: da(:)
         integer, allocatable, intent(out) :: copy(:)
         allocate( copy( size(da) ) )
         copy = da
      end subroutine foo_da

end module copy
! { dg-prune-output "compilation terminated" }
