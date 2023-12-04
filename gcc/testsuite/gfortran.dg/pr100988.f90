! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/100988 - RESTRICT was missing for optional arguments

  ! There should be restrict qualifiers for a AND b: (4 cases)
  subroutine plain (a, b)
    integer  :: a, b
    optional :: b
  end subroutine

  subroutine alloc (a, b)
    integer     :: a, b
    allocatable :: a, b
    optional    :: b
  end subroutine

  subroutine upoly (a, b)
    class(*)    :: a, b
    optional    :: b
  end subroutine

  subroutine upoly_a (a, b)
    class(*)    :: a, b
    allocatable :: a, b
    optional    :: b
  end subroutine

! { dg-final { scan-tree-dump "plain .* restrict a, .* restrict b\\)" "original" } }
! { dg-final { scan-tree-dump "alloc .* restrict a, .* restrict b\\)" "original" } }
! { dg-final { scan-tree-dump "upoly .* restrict a, .* restrict b\\)" "original" } }
! { dg-final { scan-tree-dump "upoly_a .* restrict a, .* restrict b\\)" "original" } }

  ! There should be no restrict qualifiers for the below 4 cases:
  subroutine ptr (a, b)
    integer  :: a, b
    pointer  :: a, b
    optional :: b
  end subroutine

  subroutine tgt (a, b)
    integer  :: a, b
    target   :: a, b
    optional :: b
  end subroutine

  subroutine upoly_p (a, b)
    class(*)    :: a, b
    pointer     :: a, b
    optional    :: b
  end subroutine

  subroutine upoly_t (a, b)
    class(*)    :: a, b
    target      :: a, b
    optional    :: b
  end subroutine

! { dg-final { scan-tree-dump-not "ptr .* restrict " "original" } }
! { dg-final { scan-tree-dump-not "tgt .* restrict " "original" } }
! { dg-final { scan-tree-dump-not "upoly_p .* restrict " "original" } }
! { dg-final { scan-tree-dump-not "upoly_t .* restrict " "original" } }
