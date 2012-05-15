! { dg-do run }
! { dg-options "-fdump-tree-optimized -O3" }
! Test setting host-/use-associated variables as VOLATILE
! PR fortran/30522

module impl
  implicit REAL (A-Z)
  volatile :: x
end module impl

module one
  implicit none
  logical :: l, lv
  volatile :: lv
contains
  subroutine test1(cmp)
    logical  :: cmp
    volatile :: l, lv
    if (l  .neqv. cmp) call abort()
    if (lv .neqv. cmp) call abort()
    l = .false.
    lv = .false.
    if(l .or. lv) print *, 'one_test1' ! not optimized away
  end subroutine test1
  subroutine test2(cmp)
    logical  :: cmp
    if (l  .neqv. cmp) call abort()
    if (lv .neqv. cmp) call abort()
    l = .false.
    if(l)  print *, 'one_test2_1' ! optimized away
    lv = .false.
    if(lv) print *, 'one_test2_2' ! not optimized away
  end subroutine test2
end module one

module two
  use :: one
  implicit none
  volatile :: lv,l
contains
  subroutine test1t(cmp)
    logical  :: cmp
    volatile :: l, lv
    if (l  .neqv. cmp) call abort()
    if (lv .neqv. cmp) call abort()
    l = .false.
    if(l)  print *, 'two_test1_1' ! not optimized away
    lv = .false.
    if(lv) print *, 'two_test1_2' ! not optimized away
  end subroutine test1t
  subroutine test2t(cmp)
    logical  :: cmp
    if (l  .neqv. cmp) call abort()
    if (lv .neqv. cmp) call abort()
    l = .false.
    if(l)  print *, 'two_test2_1' ! not optimized away
    lv = .false.
    if(lv) print *, 'two_test2_2' ! not optimized away
  end subroutine test2t
end module two

program main
  use :: two, only: test1t, test2t
  implicit none
  logical :: lm, lmv
  volatile :: lmv
  lm = .true.
  lmv = .true.
  call test1m(.true.)
  lm = .true.
  lmv = .true.
  call test2m(.true.)
  lm = .false.
  lmv = .false.
  call test1m(.false.)
  lm = .false.
  lmv = .false.
  call test2m(.false.)
contains
  subroutine test1m(cmp)
    use :: one
    logical  :: cmp
    volatile :: lm,lmv
    if(lm  .neqv. cmp) call abort()
    if(lmv .neqv. cmp) call abort()
    l  = .false.
    lv = .false.
    call test1(.false.)
    l  = .true.
    lv = .true.
    call test1(.true.)
    lm  = .false.
    lmv = .false.
    if(lm .or. lmv) print *, 'main_test1_1' ! not optimized away
    l   = .false.
    if(l)  print *, 'main_test1_2'          ! optimized away
    lv  = .false.
    if(lv) print *, 'main_test1_3'          ! not optimized away
    l  = .false.
    lv = .false.
    call test2(.false.)
    l  = .true.
    lv = .true.
    call test2(.true.)
  end subroutine test1m
  subroutine test2m(cmp)
    use :: one
    logical  :: cmp
    volatile :: lv
    if(lm .neqv. cmp) call abort
    if(lmv .neqv. cmp) call abort()
    l  = .false.
    lv = .false.
    call test1(.false.)
    l  = .true.
    lv = .true.
    call test1(.true.)
    lm  = .false.
    if(lm) print *, 'main_test2_1' ! not optimized away
    lmv = .false.
    if(lmv)print *, 'main_test2_2' ! not optimized away
    l   = .false.
    if(l)  print *, 'main_test2_3' ! optimized away
    lv  = .false.
    if(lv) print *, 'main_test2_4' ! not optimized away
    l  = .false.
    lv = .false.
    call test2(.false.)
    l  = .true.
    lv = .true.
    call test2(.true.)
  end subroutine test2m
end program main

! { dg-final { scan-tree-dump      "one_test1"   "optimized" } }
! TODO: dg-final { scan-tree-dump-not  "one_test2_1" "optimized" } 
! { dg-final { scan-tree-dump      "one_test2_2" "optimized" } }
! { dg-final { scan-tree-dump      "one_test2_2" "optimized" } }
! { dg-final { scan-tree-dump      "two_test2_1" "optimized" } }
! { dg-final { scan-tree-dump      "two_test2_2" "optimized" } }
! { dg-final { scan-tree-dump      "main_test1_1" "optimized" } }
! TODO: dg-final { scan-tree-dump-not  "main_test1_2" "optimized" } 
! { dg-final { scan-tree-dump      "main_test1_3" "optimized" } }
! { dg-final { scan-tree-dump      "main_test2_1" "optimized" } }
! { dg-final { scan-tree-dump      "main_test2_2" "optimized" } }
! TODO: dg-final { scan-tree-dump-not  "main_test2_3" "optimized" } 
! { dg-final { scan-tree-dump      "main_test2_4" "optimized" } }
! { dg-final { cleanup-tree-dump  "optimized" } }
