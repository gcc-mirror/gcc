! { dg-do compile }

implicit none

integer :: a, b, c

!$omp requires reverse_offload


!$omp target device (ancestor: 1)
!$omp end target

!$omp target device (ancestor : a)
!$omp end target

!$omp target device (ancestor : a + 1)
!$omp end target


! Ensure that the integer expression in the 'device' clause for
! device-modifier 'ancestor' evaluates to '1' in case of a constant.

!$omp target device (ancestor: 42)  ! { dg-error "the 'device' clause expression must evaluate to '1'" }
! !$omp end target

!$omp target device (device_num:42)
!$omp end target

!$omp target device (42)
!$omp end target

end
