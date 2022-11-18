! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/107441
! Check that procedure types and procedure decls match when the procedure
! has both character-typed and optional value args.
!
! Contributed by M.Morin

program p
  interface
    subroutine i(c, o)
      character(*) :: c
      integer, optional, value :: o
    end subroutine i
  end interface
  procedure(i), pointer :: pp
  pp => s
  call pp("abcd")
contains
  subroutine s(c, o)
    character(*) :: c
    integer, optional, value :: o
    if (present(o)) stop 1
    if (len(c) /= 4) stop 2
    if (c /= "abcd") stop 3
  end subroutine s
end program p

! { dg-final { scan-tree-dump "void s .* c, .* o, logical.* \.o, integer.* _c" "original" } }
! { dg-final { scan-tree-dump ", integer.*, logical.*, integer.* pp" "original" } }
