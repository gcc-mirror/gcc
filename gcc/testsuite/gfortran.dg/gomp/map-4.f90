!$omp target enter data device(1) if (.true.) nowait ! { dg-error "TARGET ENTER DATA must contain at least one MAP clause" }

!$omp target data device(1)  ! { dg-error "TARGET DATA must contain at least one MAP, USE_DEVICE_PTR, or USE_DEVICE_ADDR clause" }
!$omp endtarget data

!$omp target exit data device(1) if (.true.) nowait ! { dg-error "TARGET EXIT DATA must contain at least one MAP clause" }
end
