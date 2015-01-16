! { dg-do compile }
! { dg-options "" }
!
! By default, for free-form source code: Error out
!
      print *, 1                                                                                                                                          + 2 ! { dg-error "Line truncated at .1." }
      end
! { dg-excess-errors "some warnings being treated as errors" }
