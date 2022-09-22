! { dg-do compile }
! { dg-additional-options "-std=f2018 -fdump-tree-original" }
!
! PR fortran/104143
!
 interface
   subroutine foo(x)
     type(*) :: x(*)
   end
 end interface
 integer :: a
 call foo(a)
 call foo((a))
end

! { dg-final { scan-tree-dump-times "foo \\(&a\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = a;" 1 "original" } }
! { dg-final { scan-tree-dump-times "foo \\(&D.\[0-9\]+\\);" 1 "original" } }
