! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/41907
!
program test
  implicit none
  call scalar1 ()
  call assumed_shape1 ()
  call explicit_shape1 ()
contains

  ! Calling functions
  subroutine scalar1 (slr1)
    integer, optional :: slr1
    call scalar2 (slr1)
  end subroutine scalar1

  subroutine assumed_shape1 (as1)
    integer, dimension(:), optional :: as1
    call assumed_shape2 (as1)
    call explicit_shape2 (as1)
  end subroutine assumed_shape1

  subroutine explicit_shape1 (es1)
    integer, dimension(5), optional :: es1
    call assumed_shape2 (es1)
    call explicit_shape2 (es1)
  end subroutine explicit_shape1


  ! Called functions
  subroutine assumed_shape2 (as2)
    integer, dimension(:),optional :: as2
    if (present (as2)) STOP 1
  end subroutine assumed_shape2

  subroutine explicit_shape2 (es2)
    integer, dimension(5),optional :: es2
    if (present (es2)) STOP 2
  end subroutine explicit_shape2

  subroutine scalar2 (slr2)
    integer, optional :: slr2
    if (present (slr2)) STOP 3
  end subroutine scalar2

end program test

! { dg-final { scan-tree-dump-times "scalar2 \\(slr1" 1 "original" } }

! { dg-final { scan-tree-dump-times "= es1 != 0B" 1 "original" } }
! { dg-final { scan-tree-dump-times "assumed_shape2 \\(es1" 0 "original" } }
! { dg-final { scan-tree-dump-times "explicit_shape2 \\(es1" 1 "original" } }

! { dg-final { scan-tree-dump-times "= as1 != 0B" 2 "original" } }
! { dg-final { scan-tree-dump-times "assumed_shape2 \\(as1" 0 "original" } }
! { dg-final { scan-tree-dump-times "explicit_shape2 \\(as1" 0 "original" } }

