! { dg-do compile }

implicit none

integer :: n

!$omp target device (1)
!$omp end target

!$omp target device (n)
!$omp end target

!$omp target device (n + 1)
!$omp end target

!$omp target device (device_num : 1)
!$omp end target

!$omp target device (device_num : n)
!$omp end target

!$omp target device (device_num : n + 1)
!$omp end target

!$omp target device (invalid : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device ( : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device ( , : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (ancestor, device_num : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (ancestor, device_num, ancestor : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (device_num device_num : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (ancestor device_num : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (device_num, invalid : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (ancestor, invalid : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (ancestor, , , : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (invalid, ancestor : 1)  ! { dg-error "xpected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (invalid, invalid, ancestor : 1)  ! { dg-error "xpected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (device_num invalid : 1)  ! { dg-error "Expected integer expression or a single device-modifier 'device_num' or 'ancestor' at" }
! !$omp end target

!$omp target device (device_num : n, n)  ! { dg-error "Expected integer expression" }
! !$omp end target

end
