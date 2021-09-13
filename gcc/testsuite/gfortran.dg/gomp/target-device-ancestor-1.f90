! { dg-do compile }

! Ensure that a 'requires' directive with the 'reverse_offload' clause was
! specified.

!$omp target device (ancestor:1)  ! { dg-error "'ancestor' device modifier not preceded by 'requires' directive with 'reverse_offload' clause" }
! !$omp end target

end