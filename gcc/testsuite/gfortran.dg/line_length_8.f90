! { dg-do compile }
! { dg-options "-std=f2018 -Wline-truncation" }
!
! By default, for free-form source code: Error out
! Even with -Wline-truncation, we still get an error
!
      print *, 1                                                                                                                                          + 2 ! { dg-error "Line truncated at .1." }
      end
! { dg-prune-output "some warnings being treated as errors" }
