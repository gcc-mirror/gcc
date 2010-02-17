! { dg-do compile }
! { dg-options "-O2" }
      subroutine foo
      implicit none

      integer :: i

      call gee_i(int(i**huge(0_8),kind=kind(i)))

      end subroutine foo

