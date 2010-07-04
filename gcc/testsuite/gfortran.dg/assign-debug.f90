! { dg-do compile }
! { dg-options "-fcompare-debug -O2" }
      program test
      integer i
      common i
      assign 2000 to i  ! { dg-warning "Deleted feature: ASSIGN statement" }
2000  continue
      end
