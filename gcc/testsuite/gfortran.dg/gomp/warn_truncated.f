! { dg-do compile }
! { dg-additional-options "-Wall" }
!
! PR fortran/94709
!
! Check that 'Line truncated' gets printed as appropriate
!
c$omp target                                                            ! map()
c$omp end target

c$omp target                                                            map() ! { dg-warning "Line truncated" }
c$acc declare                                                           map()
ca =                                                                    5
c$omp end target
       end
