! { dg-do compile }
!
! PR fortran/53985
!
! Check that the (default) -Wno-c-binding-type works
! and no warning is printed.
!
! With -Wc-binding-type, one gets:
!  Warning: Variable 'x' at (1) is a dummy argument to the BIND(C) procedure
!           'test' but may not be C interoperable )
!
subroutine test(x) bind(C)
  integer :: x
end subroutine test
