module m
 implicit none
 integer :: ii
 integer :: x, y(20), z, v, u, k

 common /b_ii/ ii
 common /b_x/ x  ! { dg-message "sorry, unimplemented: 'b_x' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" }
 common /b_y/ y ! { dg-message "sorry, unimplemented: 'b_y' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" }
 common /b_z/ z ! { dg-message "sorry, unimplemented: 'b_z' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" }
 common /b_v/ v
 common /b_u/ u ! { dg-message "sorry, unimplemented: 'b_u' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" }
 common /b_k/ k ! { dg-message "sorry, unimplemented: 'b_k' with 'omp groupprivate' on the host; try 'device_type\\(nohost\\)'" }

 ! { dg-prune-output "sorry, unimplemented: 'b_v' with 'omp groupprivate' on devices other than" }

 !$omp groupprivate(/b_x/, /b_z/) device_Type( any )
 !$omp declare target local(/b_x/) device_type ( any )
 !$omp declare target enter( /b_ii/) ,local(/b_y/), device_type ( host )
 !$omp groupprivate(/b_y/) device_type( host)
 !$omp groupprivate(/b_v/) device_type (nohost )
 !$omp groupprivate(/b_u/)
 !$omp groupprivate(/b_k/)
end module
