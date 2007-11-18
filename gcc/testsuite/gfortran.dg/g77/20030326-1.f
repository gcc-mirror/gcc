! { dg-do compile }
! { dg-options -pedantic }
!      PR fortran/9793
!      larson@w6yx.stanford.edu
!
! For gfortran, see PR 13490
!
       integer c
       c = -2147483648_4 / (-1) ! { dg-error "too big for its kind" "" }
       end
