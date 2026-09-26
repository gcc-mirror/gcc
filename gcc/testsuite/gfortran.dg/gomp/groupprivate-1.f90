module m
 implicit none
 integer :: ii
 integer :: x, y(20), z, v, u, k
 ! { dg-message "sorry, unimplemented: 'k' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" "" { target *-*-* } .-1 }
 ! { dg-message "sorry, unimplemented: 'u' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" "" { target *-*-* } .-2 }
 ! { dg-message "sorry, unimplemented: 'x' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" "" { target *-*-* } .-3 }
 ! { dg-message "sorry, unimplemented: 'y' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" "" { target *-*-* } .-4 }
 ! { dg-message "sorry, unimplemented: 'z' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" "" { target *-*-* } .-5 }

 ! { dg-prune-output "sorry, unimplemented: 'v' with 'omp groupprivate' on devices other than" }

 !$omp groupprivate(x, z) device_Type( any )
 !$omp declare target local(x) device_type ( any )
 !$omp declare target enter( ii) ,local(y), device_type ( host )
 !$omp groupprivate(y) device_type( host)
 !$omp groupprivate(v) device_type (nohost )
 !$omp groupprivate(u)

 !$omp groupprivate(k)
end module
