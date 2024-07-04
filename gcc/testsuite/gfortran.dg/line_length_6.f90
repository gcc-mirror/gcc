! { dg-do compile }
! { dg-options "-std=f2018" }
!
! By default, for free-form source code: Error out
!
      print *, 1                                                                                                                                          + 2 ! { dg-error "Line truncated at .1." }
      end
! { dg-prune-output "some warnings being treated as errors" }
