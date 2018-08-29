! { dg-do compile }
! PR fortran/84922
! This should compile without error.
module foom

   implicit none

   interface foo
      module procedure foo_sngl
      module procedure foo_dble
   end interface foo

   contains

      subroutine foo_sngl(n, f, g, h)
         integer n
         real f, g, h
      end subroutine foo_sngl

      subroutine foo_dble(n, f, g, h)
         integer n
         double precision f, g, h
      end subroutine foo_dble

end module foom
