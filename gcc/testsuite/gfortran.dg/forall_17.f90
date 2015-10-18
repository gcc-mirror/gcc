! { dg-do compile }
! { dg-options "-ffrontend-optimize" }
! PR fortran/66385 - this used to ICE
! Original test case by Mianzhi Wang
program test
  double precision::aa(30)
  double precision::a(3,3),b
  b=1d0
  forall(i=1:3)
    a(:,i)=b*[1d0,2d0,3d0]
  end forall

  forall(i=1:10)
    aa(10*[0,1,2]+i)=1d0
  end forall

end program
