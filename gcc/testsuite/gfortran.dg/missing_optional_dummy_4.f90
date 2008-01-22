! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/34848
! 
! The "0" for the string size of the absent optional
! argument was missing.
!
module krmod
contains
 subroutine doit()
   implicit none
    real :: doit1
    doit1 = tm_doit()
   return
 end subroutine doit
 function tm_doit(genloc)
   implicit none
   character, optional  :: genloc
   real :: tm_doit
   tm_doit = 42.0 
 end function tm_doit
end module krmod

! { dg-final { scan-tree-dump " tm_doit \\(0B, 0\\);" "original" } }
! { dg-final { cleanup-tree-dump "original" } }
! { dg-final { cleanup-modules "krmod" } }

