! { dg-do compile }
! { dg-options "-Wno-error" }
!
! By default, for free-form source code: Error out
! But due to -Wno-error, we only expect a warning
!
      print *, 1                                                                                                                                          + 2 ! { dg-warning "Line truncated at .1." }
      end
