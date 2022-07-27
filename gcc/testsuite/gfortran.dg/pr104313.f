! { dg-do compile }
! { dg-additional-options "-ff2c -fdump-tree-original" }
!
! PR fortran/104313 - ICE verify_gimple failed with -ff2c
! Contributed by G.Steinmetz

      function f(a)
      return
      end

! { dg-final { scan-tree-dump-times "return" 1 "original" } }
