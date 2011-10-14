! { dg-do run }
! { dg-options "-fcheck=all" }
!
! { dg-shouldfail "Pointer check" }
! { dg-output "Fortran runtime error: Pointer actual argument 'p' is not associated" }
!
! PR fortran/50718
!
! Was failing with -fcheck=pointer: Segfault at run time

integer, pointer :: p => null()

call sub2(%val(p)) ! Invalid: Nonassociated pointer
end

! Not quite correct dummy, but if one uses VALUE, gfortran
! complains about a missing interface - which we cannot use
! if we want to use %VAL().

subroutine sub2(p)
  integer :: p
end subroutine sub2
