! { dg-do compile }
!
! PR 50379: ICE in gfc_typenode_for_spec at fortran/trans-types.c
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

  function f() result(res)
    interface res          ! { dg-error "attribute conflicts with" }
  end
