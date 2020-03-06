! PR target/91913
! { dg-do compile }
! { dg-options "-std=legacy -Ofast --param max-cse-insns=0 -fno-schedule-insns -fsanitize=null" }

include 'string_ctor_1.f90'
