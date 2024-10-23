! { dg-options "-fdiagnostics-format=sarif-file -fmax-errors=1 -Wfatal-errors" }

program main
    implicit none
    print*, "Hello World!"
end program main

! Verify that some JSON was written to a file with the expected name.
! { dg-final { verify-sarif-file } } */

! We expect a successful invocation and no results:
! { dg-final { scan-sarif-file "\"executionSuccessful\": true" } }
! { dg-final { scan-sarif-file "\"results\": \\\[\\\]" } }
