! Testcase for the COMPLEX intrinsic
! { dg-do compile }
  complex c
  c = complex(.true.,1.0) ! { dg-error "must be INTEGER or REAL" }
  c = complex(1) ! { dg-error "Missing actual argument" }
  c = complex(1,c) ! { dg-error "must be INTEGER or REAL" }
  end
