! PR tree-optimization/87360
! { dg-do compile }
! { dg-options "-fno-tree-dce -O3 --param max-completely-peeled-insns=0" }

include 'function_optimize_2.f90'
