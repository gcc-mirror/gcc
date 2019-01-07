! { dg-do compile }
!
! PR 88009: [9 Regression] ICE in find_intrinsic_vtab, at fortran/class.c:2761
!
! Contributed by G. Steinmetz <gscfq@t-online.de>

module m
   class(*), allocatable :: z
end
block data
   use m
   z = 'z'  ! { dg-error "assignment statement is not allowed|Unexpected assignment statement" }
end
