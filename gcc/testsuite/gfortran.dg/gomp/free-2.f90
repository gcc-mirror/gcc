! { dg-do compile }
!
! PR fortran/33445
!
!$OMP&foo ! { dg-warning "starts a commented line" }
!
!$OMP parallel 
!$OMP& default(shared) ! { dg-warning "starts a commented line" }
!$OMP end parallel
!
!$OMP parallel 
!$OMP+ default(shared) ! { dg-warning "starts a commented line" }
!$OMP end parallel
       end
