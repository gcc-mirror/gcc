! { dg-do compile }
!
! PR 50070: Segmentation fault at size_binop_loc in fold-const.c
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

subroutine sub
  common n,z             ! { dg-error "must have constant character length" }
  integer :: n
  character(len=n) :: z
end
