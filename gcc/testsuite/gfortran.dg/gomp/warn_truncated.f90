! { dg-do compile }
! { dg-additional-options "-std=f2018" }
!
! PR fortran/94709
!
! Check that 'Line truncated' gets printed as appropriate
!
integer i
i = 0
!$omp target                                                                                                                                        ! map()
!$omp end target

!$omp target                                                                                                                                        map() ! { dg-error "Line truncated" }
!$acc kernels                                                                                                                                       copy()
!a =                                                                                                                                                 5
!$acc end kernels
!$omp end target
end

! { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 }
