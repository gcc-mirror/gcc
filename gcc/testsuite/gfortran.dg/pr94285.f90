! PR debug/94285
! { dg-do compile }
! { dg-options "-Os -fno-tree-dominator-opts -fno-tree-vrp -fcompare-debug" }

include 'array_constructor_40.f90'
