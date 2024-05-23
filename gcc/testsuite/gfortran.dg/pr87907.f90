! { dg-do compile }
! PR fortran/pr87907
! Original testcase contributed by Gerhard Stienmetz <gscfq at t-online dot de>
module m
   interface
      module function g(x) result(z)
         integer, intent(in) :: x
         integer, allocatable :: z
      end
   end interface
end

submodule(m) m2
   contains
      subroutine g(x) ! { dg-error "FUNCTION attribute conflicts with SUBROUTINE" }
      end
end

program p
   use m
   integer :: x = 3
   call g(x)
end

! { dg-prune-output "Two main PROGRAMs" }
