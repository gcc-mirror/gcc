module m
 implicit none
 integer :: ii
 integer :: x, y(20), z, v, u, k  ! { dg-warning "Ignoring the 'groupprivate' attribute for 'threadprivate' variable 'k' declared at .1. \\\[-Wopenmp\\\]" }
! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by 'x' declared at .1." "" { target *-*-* } .-1 }
! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by 'y' declared at .1." "" { target *-*-* } .-2 }
! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by 'z' declared at .1." "" { target *-*-* } .-3 }
! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by 'v' declared at .1." "" { target *-*-* } .-4 }
! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by 'u' declared at .1." "" { target *-*-* } .-5 }
!
! Note:Error different as 'groupprivate' flag is overwritten by 'threadprivate', cf. warning above.
! { dg-error "Sorry, OMP DECLARE TARGET with LOCAL clause not implemented, used by 'k' declared at .1." "" { target *-*-* } .-8 }
 !$omp groupprivate(x, z) device_Type( any )
 !$omp declare target local(x) device_type ( any )
 !$omp declare target enter( ii) ,local(y), device_type ( host )
 !$omp groupprivate(y) device_type( host)
 !$omp groupprivate(v) device_type (nohost )
 !$omp groupprivate(u)

 ! See also (currently unresolved) OpenMP Specification Issue 4663.
 !$omp groupprivate(k)
 !$omp threadprivate(k)
end module
