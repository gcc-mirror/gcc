! { dg-do compile }
! { dg-options "-Wunused-variable -Wunused-parameter" }
! This tests the fix for PR18111 in which some artificial declarations
! were being listed as unused parameters:
! (i) Array dummies, where a copy is made;
! (ii) The dummies of "entry thunks" (ie. the articial procedures that
! represent ENTRYs and call the "entry_master" function; and
! (iii) The __entry parameter of the entry_master function, which
! indentifies the calling entry thunk.
! All of these have DECL_ARTIFICIAL (tree) set.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module foo
  implicit none
contains

!This is the original problem

  subroutine bar(arg1, arg2, arg3, arg4, arg5)
    character(len=80), intent(in) :: arg1
    character(len=80), dimension(:), intent(in) :: arg2
    integer, dimension(arg4), intent(in) :: arg3
    integer, intent(in) :: arg4
    character(len=arg4), intent(in) :: arg5
    print *, arg1, arg2, arg3, arg4, arg5
  end subroutine bar

! This ICED with the first version of the fix because gfc_build_dummy_array_decl
! sometimes NULLS sym->backend_decl; taken from aliasing_dummy_1.f90

  subroutine foo1 (slist, i)
    character(*), dimension(*) :: slist
    integer i
    write (slist(i), '(2hi=,i3)') i
  end subroutine foo1

! This tests the additions to the fix that prevent the dummies of entry thunks
! and entry_master __entry parameters from being listed as unused.

  function f1 (a)
    integer, dimension (2, 2) :: a, b, f1, e1
    f1 (:, :) = 15 + a
    return
  entry e1 (b)
    e1 (:, :) = 42 + b
  end function

end module foo
