module m
 implicit none
 integer :: ii
 integer :: x, y(20), z, v, u, k

 common /b_ii/ ii
 common /b_x/ x  ! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by common block '/b_x/' declared at .1." }
 common /b_y/ y  ! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by common block '/b_y/' declared at .1." }
 common /b_z/ z  ! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by common block '/b_z/' declared at .1." }
 common /b_v/ v  ! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by common block '/b_v/' declared at .1." }
 common /b_u/ u  ! { dg-error "Sorry, OMP GROUPPRIVATE not implemented, used by common block '/b_u/' declared at .1." }
 common /b_k/ k  ! { dg-warning "Ignoring the 'groupprivate' attribute for 'threadprivate' common block '/b_k/' declared at .1. \\\[-Wopenmp\\\]" }
! { dg-error "Sorry, OMP DECLARE TARGET with LOCAL clause not implemented, used by common block '/b_k/' declared at .1." "" { target *-*-* } .-1 }

 !$omp groupprivate(/b_x/, /b_z/) device_Type( any )
 !$omp declare target local(/b_x/) device_type ( any )
 !$omp declare target enter( /b_ii/) ,local(/b_y/), device_type ( host )
 !$omp groupprivate(/b_y/) device_type( host)
 !$omp groupprivate(/b_v/) device_type (nohost )
 !$omp groupprivate(/b_u/)

 ! See also (currently unresolved) OpenMP Specification Issue 4663.
 !$omp groupprivate(/b_k/)
 !$omp threadprivate(/b_k/)
end module
