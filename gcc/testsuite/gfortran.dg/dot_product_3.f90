! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR 61999 - this used to ICE.
! Original test case by A. Kasahara
program main
   use, intrinsic:: iso_fortran_env, only: output_unit

   implicit none

   write(output_unit, *) dot_product([1, 2], [2.0, 3.0])

   stop
end program main
! { dg-final { scan-tree-dump-times "8\\.0e\\+0" 1 "original" } }
