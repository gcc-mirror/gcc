! PR middle-end/49675
! { dg-do compile }
! { dg-options "-finstrument-functions" }
end
! { dg-final { scan-assembler "__cyg_profile_func_enter" } }
! { dg-final { scan-assembler "__cyg_profile_func_exit" } }
