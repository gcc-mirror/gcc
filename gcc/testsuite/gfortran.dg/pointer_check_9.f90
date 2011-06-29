! { dg-do run }
! { dg-options "-fcheck=all -std=f2008 -fall-intrinsics" }
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
