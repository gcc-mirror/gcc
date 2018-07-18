! { dg-do compile }
! PR fortran/69741 - improve error message for nonscalar FORALL index variables
!
subroutine check
  integer :: ii(2), i
  real :: a(3,2)

  forall (ii(1)=1:3, i=1:2) ! { dg-error "scalar variable of type integer" }
     a(ii(1),i) = ii(1) * i
  end forall

  forall (j=1:3, ii(2)=1:2) ! { dg-error "scalar variable of type integer" }
     a(j,ii(2)) = j * ii(2)
  end forall

end subroutine check
