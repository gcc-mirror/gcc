! { dg-do run }
! PR 84394 - this used to complain about private procedures in
! BLOCK data.
module mod1
   implicit none
   type :: type1
      integer :: i1
   end type type1
end module

module mod2
   implicit none
   contains
      subroutine sub1
         integer vals
         common /block1/ vals(5)
         if (any(vals /= [1, 2, 3, 4, 5])) stop 1
      end subroutine
end module

block data blkdat
  use mod1
  integer vals
  common /block1/ vals(5)
  data vals/1, 2, 3, 4, 5/
end block data blkdat

program main
  use mod2, only: sub1
  implicit none
  call sub1
end program

