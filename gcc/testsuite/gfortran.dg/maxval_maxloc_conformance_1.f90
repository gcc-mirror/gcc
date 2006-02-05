! { dg-do compile }
! PR 26039:  Tests for different ranks for (min|max)loc, (min|max)val, product
!            and sum were missing.
program main
  integer, dimension(2) :: a
  logical, dimension(2,1) :: lo
  logical, dimension(3) :: lo2
  a = (/ 1, 2 /)
  lo = .true.
  print *,minloc(a,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,maxloc(a,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,minval(a,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,maxval(a,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,sum(a,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,product(a,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,minloc(a,1,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,maxloc(a,1,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,minval(a,1,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,maxval(a,1,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,sum(a,1,mask=lo) ! { dg-error "Incompatible ranks" }
  print *,product(a,1,mask=lo) ! { dg-error "Incompatible ranks" }

  print *,minloc(a,mask=lo2) ! { dg-error "different shape" }
  print *,maxloc(a,mask=lo2) ! { dg-error "different shape" }
  print *,minval(a,mask=lo2) ! { dg-error "different shape" }
  print *,maxval(a,mask=lo2) ! { dg-error "different shape" }
  print *,sum(a,mask=lo2) ! { dg-error "different shape" }
  print *,product(a,mask=lo2) ! { dg-error "different shape" }
  print *,minloc(a,1,mask=lo2) ! { dg-error "different shape" }
  print *,maxloc(a,1,mask=lo2) ! { dg-error "different shape" }
  print *,minval(a,1,mask=lo2) ! { dg-error "different shape" }
  print *,maxval(a,1,mask=lo2) ! { dg-error "different shape" }
  print *,sum(a,1,mask=lo2) ! { dg-error "different shape" }
  print *,product(a,1,mask=lo2) ! { dg-error "different shape" }
end program main
