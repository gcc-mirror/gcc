! { dg-do compile }
! Check for a few restrictions on the back argument to
! minloc and maxloc.
program main
  integer, dimension(3) :: a
  a = [1,2,3]
  print *,minloc(a,back=42) ! { dg-error "must be LOGICAL" }
  print *,minloc(a,back=[.true.,.false.]) ! { dg-error "must be a scalar" }
  print *,maxloc(a,back=42) ! { dg-error "must be LOGICAL" }
  print *,maxloc(a,back=[.true.,.false.]) ! { dg-error "must be a scalar" }
end program main
