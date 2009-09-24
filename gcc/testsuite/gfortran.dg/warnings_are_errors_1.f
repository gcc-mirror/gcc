! { dg-do compile }
! { dg-options " -Werror" }
! PR fortran/21061
! gfortran ignores -Werror
! fixed-form tests
       program warnings_are_errors_1
       implicit none
       integer(kind=1) :: i
       real :: r1, r2(3)
! gfc_warning_now:
0      r1 = 0 ! { dg-warning "Zero is not a valid statement label" }
!
34 5   i=0 
! gfc_notify_std(GFC_STD_F95_DEL):
       do r1 = 1, 2 ! { dg-error "Deleted feature: Loop variable" }
         i = i+1
       end do
       call foo j bar
! gfc_warning:
       r2(4) = 0 ! { dg-warning "is out of bounds" }
       
       goto 3 45
       end
! { dg-final { output-exists-not } }
