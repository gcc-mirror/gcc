! { dg-do compile }
! { dg-options "-Wno-line-truncation" }
!
! By default, for free-form source code: Error out
! But due to the explicit -Wno-line-truncation, compile w/o warning
!
      print *, 1                                                                                                                                          + 2
      end
