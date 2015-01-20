! { dg-do compile } 

program test
  ! only one directive-name may appear in directive
  !$acc parallel kernels ! { dg-error "Unclassifiable OpenACC directive" }
end