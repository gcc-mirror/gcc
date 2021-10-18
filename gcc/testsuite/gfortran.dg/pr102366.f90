! { dg-do compile }
! { dg-options "-fdump-tree-original -Wall" }
! { dg-final { scan-tree-dump-times "static real" 1 "original" } }
! PR fortran/102366 - large arrays no longer become static

program p
  real(kind=4) :: a(16776325)
  a=1.0
end
