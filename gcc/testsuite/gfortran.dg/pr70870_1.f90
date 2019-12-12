! { dg-do compile }
! { dg-options "-std=gnu" }
! PR fortran/70870
! Contributed by Vittorio Zecca <zeccav at gmail dot com >
      type t
       integer :: g=0   ! default initialization
      end type
      type(t) :: v2
      data v2/t(2)/     ! { dg-error "default initialization shall not" }
      end
