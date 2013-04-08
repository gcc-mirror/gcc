! { dg-do compile }
! { dg-options "-std=gnu" }
!
! Tests the fix for PR30237 in which alternate returns in intrinsic
! actual arglists were quietly ignored.
!
! Contributed by Brooks Moses <brooks@gcc.gnu.org>
!
program ar1
    interface random_seed
      subroutine x (a, *)
        integer a
      end subroutine x
    end interface random_seed

    real t1(2)
    call cpu_time(*20)        ! { dg-error "not permitted" }
    call cpu_time(*20, t1(1)) ! { dg-error "Too many arguments" }
! This specific version is permitted by the generic interface.
    call random_seed(i, *20)
! The new error gets overwritten but the diagnostic is clear enough.
    call random_seed(i, *20, *30) ! { dg-error "not consistent" }
    stop
20  write(*,*) t1
30 stop
end
