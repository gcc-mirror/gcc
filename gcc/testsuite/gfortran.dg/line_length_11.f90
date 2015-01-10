! { dg-do compile }
! { dg-options "-Wno-all" }
!
! By default, for free-form source code: Error out
! But due to the explicit -Wno-all, compile w/o warning
!
      print *, 1                                                                                                                                          + 2
      end
