! PR tree-optimization/90385
! { dg-do compile }
! { dg-require-effective-target pthread }
! { dg-options "-O1 -ftree-parallelize-loops=2 -fno-tree-ccp -fno-tree-ch -fno-tree-copy-prop -fno-tree-forwprop -fno-tree-sink --param parloops-min-per-thread=5" }

include 'array_constructor_47.f90'
