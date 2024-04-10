! { dg-do compile }
! { dg-options "-Werror -Wall" }
module foo
   contains
      subroutine bar 
         character(len=:), allocatable :: s(:)
         call bah(s)
      end subroutine bar
end module foo
