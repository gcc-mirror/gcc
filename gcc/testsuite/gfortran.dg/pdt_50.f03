! { dg-do compile }
! ! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR102241, which caused an ICE in gfc_get_derived_type.
! The test in comment 4 used to cause a spurious error.
!
! Contributed by Roland Wirth  <roland_wirth@web.de>
!
    MODULE mo
      TYPE t1(n)
        INTEGER, LEN :: n
        INTEGER :: a(n)
      END TYPE

      TYPE t2
        TYPE(t1(:)), allocatable :: p_t1
      END TYPE
    END MODULE

!---Check test in comment 4 now works---
    MODULE mo2
      TYPE u1(n)
        INTEGER, LEN :: n
        INTEGER :: a(n)
      END TYPE

      TYPE u2
        TYPE(u1(2)), POINTER :: p_u1
      END TYPE

    CONTAINS

      SUBROUTINE sr

        type(u1(2)), target :: tgt
        type(u2) :: pt

        tgt = u1(2)([42,84])
        pt%p_u1 => tgt
        if (any (pt%p_u1%a /= [42,84])) stop 1
      END SUBROUTINE
    END MODULE
!------

    use mo
    use mo2
    type(t2) :: d
    d%p_t1 = t1(8)([42,43,44,45,42,43,44,45])
    if (any (d%p_t1%a /= [42,43,44,45,42,43,44,45])) stop 2
    call sr
    deallocate (d%p_t1)
end
! { dg-final { scan-tree-dump-times "__builtin_malloc" 8 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 9 "original" } }
