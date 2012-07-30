! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/44354
! array constructors were giving unexpected results when the ac-implied-do
! variable was used in one of the ac-implied-do bounds.
!
! Original testcase by Vittorio Zecca <zeccav@gmail.com>
!
      I=5
      print *,(/(i,i=I,8)/) ! { dg-error "initial expression references control variable" }
      print *,(/(i,i=1,I)/) ! { dg-error "final expression references control variable" }
      print *,(/(i,i=1,50,I)/) ! { dg-error "step expression references control variable" }
      end


