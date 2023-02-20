! { dg-do compile }
! { dg-options "-O2" }

subroutine bar
!GCC$ ATTRIBUTES noreturn :: foo1
call foo1()
call noreturn_autodetection_failed()
end subroutine
! /* { dg-final { scan-assembler-not "noreturn_autodetection_failed" } } */
