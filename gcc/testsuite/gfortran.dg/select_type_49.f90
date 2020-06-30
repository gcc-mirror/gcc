! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR 95366 - this did not work due the wrong hashes
! being generated for CHARACTER variables.
MODULE mod1
  implicit none
  integer :: tst(3)
CONTAINS
  subroutine showpoly(poly)
    CLASS(*), INTENT(IN) :: poly(:)
    SELECT TYPE (poly)
    TYPE IS(INTEGER)
       tst(1) = tst(1) + 1
    TYPE IS(character(*))
       tst(2) = tst(2) + 1
    class default
       tst(3) = tst(3) + 1
    end select
  end subroutine showpoly
END MODULE mod1
MODULE mod2
  implicit none
CONTAINS
subroutine polytest2()
   use mod1
   integer :: a(1)
   character(len=42) :: c(1)
   call showpoly(a)
   if (any(tst /= [1,0,0])) stop 1
   call showpoly(c)
   if (any(tst /= [1,1,0])) stop 2
end subroutine polytest2
END MODULE mod2
PROGRAM testpoly
  use mod2
  CALL polytest2()
END PROGRAM testpoly
! The value of the hashes are also checked.  If you get
! a failure here, be aware that changing that value is
! an ABI change.

! { dg-final { scan-tree-dump-times "== 17759" 1 "original" } }  
! { dg-final { scan-tree-dump-times "== 85893463" 1 "original" } }
