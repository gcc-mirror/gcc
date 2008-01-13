! { dg-do compile }
!
! PR fortran/34759
! gfortran was before rejecting passing an assumed-size array
! where the last dimension was specified.
!
! Test case provided by Dick Hendickson.
!
       subroutine j_assumed_size(A,N)
       dimension A(10,11,12,*), k(3), l(3), m(4)
       m = shape(A(:,:,:,:N)) ! OK
       l = shape(A(:,:,:,3))  ! OK
       m = shape(A(:,:,:,:))  ! { dg-error "upper bound of assumed size array" }
       m = shape(A) ! { dg-error "must not be an assumed size array" }
       end
