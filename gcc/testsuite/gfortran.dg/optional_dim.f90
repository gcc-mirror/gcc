! { dg-do compile }
subroutine foo(a,n)
  real, dimension(2) :: a
  integer, optional :: n
  print *,maxloc(a,dim=n) ! { dg-error "must not be OPTIONAL" }
  print *,maxloc(a,dim=4) ! { dg-error "is not a valid dimension index" }
  print *,maxval(a,dim=n) ! { dg-error "must not be OPTIONAL" }
  print *,maxval(a,dim=4) ! { dg-error "is not a valid dimension index" }
end subroutine foo
  
