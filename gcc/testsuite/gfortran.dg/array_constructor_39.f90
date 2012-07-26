! { dg-do run }
!
! PR fortran/44354
! array constructors were giving unexpected results when the ac-implied-do
! variable was used in one of the ac-implied-do bounds.
!
! Original testcase by Vittorio Zecca <zeccav@gmail.com>
!
      I=5
      if (any((/(i,i=1,I)/) /= (/1,2,3,4,5/))) call abort ! { dg-warning "final expression references control variable" }
      if (I /= 5) call abort
      end

