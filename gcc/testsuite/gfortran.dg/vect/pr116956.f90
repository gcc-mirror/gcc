! { dg-do compile }
! { dg-require-effective-target vect_int }
! { dg-additional-options "-mcpu=neoverse-v2 -Ofast" { target aarch64*-*-* } }

SUBROUTINE nesting_offl_init(u, v, mask)
       IMPLICIT NONE
       real :: u(:)
       real :: v(:)
       integer :: mask(:)
       u = MERGE( u, v, BTEST (mask, 1) )
END SUBROUTINE nesting_offl_init
