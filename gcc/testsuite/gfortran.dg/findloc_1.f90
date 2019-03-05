! { dg-do compile }
! Test errors in findloc.
program main
  integer, dimension(4) :: a
  logical, dimension(3) :: msk
  a = [2,4,6,8]
  print *,findloc(a) ! { dg-error "Missing actual argument" }
  print *,findloc(a,value=.true.) ! { dg-error "must be in type conformance to argument" }
  print *,findloc(a,23,dim=6) ! { dg-error "is not a valid dimension index" }
  print *,findloc(a,-42,dim=2.0) ! { dg-error "must be INTEGER" }
  print *,findloc(a,6,msk) ! { dg-error "Different shape for arguments 'array' and 'mask'" }
  print *,findloc(a,6,kind=98) ! { dg-error "Invalid kind for INTEGER" }
end program main
