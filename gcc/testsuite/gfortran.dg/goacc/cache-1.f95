! { dg-do compile }
! { dg-additional-options "-std=f2008" }

program test
  implicit none
  integer :: i, d(10)

  do concurrent (i=1:5)
    !$acc cache (d)
  enddo
end
! { dg-prune-output "unimplemented" }
