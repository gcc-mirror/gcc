! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }
!
! PR fortran/47266
!
! Check whether the private procedure "priv" is optimized away
!
module m
  implicit none
  private :: priv
  private :: export1, export2
  public  :: pub
contains
  integer function priv()
    priv = 44
  end function priv
  integer function export1()
    export1 = 45
  end function export1
  function export2() bind(C) ! { dg-warning "is marked PRIVATE" }
    use iso_c_binding, only: c_int
    integer(c_int) :: export2
    export2 = 46
  end function export2
  subroutine pub(a,b)
    integer :: a
    procedure(export1), pointer :: b
    a = priv()
    b => export1
  end subroutine pub
end module m
! { dg-final { scan-tree-dump-times "priv" 0 "optimized" } }
! { dg-final { scan-tree-dump-times "export1 \\(\\)" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "export2 \\(\\)" 1 "optimized" } }
