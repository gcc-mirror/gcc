! { dg-do run }
! { dg-options "-fcheck=all -std=f2003 -fall-intrinsics" }
! { dg-shouldfail "Pointer actual argument 'ptr' is not associated" }
!
! PR fortran/49255
!
! Valid F2008, invalid F95/F2003.
!
integer,pointer :: ptr => null()
call foo (ptr)
contains
  subroutine foo (x)
    integer, optional :: x
    if (present (x)) call abort ()
  end subroutine foo
end
