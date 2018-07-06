! { dg-do compile }
! { dg-options "-fgraphite-identity -O1 --param sccvn-max-alias-queries-per-access=0" }
   include "../assumed_rank_bounds_2.f90"
