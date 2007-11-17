! { dg-do compile }
!
! Check for diagnostics (PR 34108)
  write (*,0) 'xxx' ! { dg-error "Statement label .* is zero" }
  write (*,1) 'xxx' ! { dg-error "FORMAT label .* not defined" }
  write (*,123456) 'xxx' ! { dg-error "Too many digits in statement label" }
  write (*,-1) 'xxx' ! { dg-error "" }
  end
