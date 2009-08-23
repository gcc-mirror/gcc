! { dg-do compile }
! { dg-options -std=f95 }
!
! Code was posted to comp.lang.fortran by Richard Maine.
! http://groups.google.com/group/comp.lang.fortran/browse_frm/thread/fff9b3426211c018#
!
module m
  type :: foo
    real, pointer :: array(:)
    procedure (), pointer, nopass :: f ! { dg-error "Procedure pointer component" }
  end type
contains
    elemental subroutine fooAssgn (a1, a2)
        type(foo), intent(out) :: a1
        type(foo), intent(in) :: a2
        allocate (a1%array(size(a2%array)))

        a1%array = a2%array
        a1%f => a2%f         ! { dg-error "not a member of the" }
    end subroutine
end module m
