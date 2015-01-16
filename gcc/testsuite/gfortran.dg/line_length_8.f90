! { dg-do compile }
! { dg-options "-Wline-truncation" }
!
! By default, for free-form source code: Error out
! Even with -Wline-truncation, we still get an error
!
      print *, 1                                                                                                                                          + 2 ! { dg-error "Line truncated at .1." }
      end
! { dg-excess-errors "some warnings being treated as errors" }
