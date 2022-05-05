! { dg-do compile }
! PR fortran/103777 - ICE in gfc_simplify_maskl
! Contributed by G.Steinmetz

program p
  print *, maskl([999])       ! { dg-error "must be less than or equal" }
  print *, maskr([999])       ! { dg-error "must be less than or equal" }
  print *, maskl([-999])      ! { dg-error "must be nonnegative" }
  print *, maskr([-999])      ! { dg-error "must be nonnegative" }
  print *, maskl([32],kind=4)
  print *, maskl([33],kind=4) ! { dg-error "must be less than or equal" }
  print *, maskl([64],kind=8)
  print *, maskl([65],kind=8) ! { dg-error "must be less than or equal" }
end
