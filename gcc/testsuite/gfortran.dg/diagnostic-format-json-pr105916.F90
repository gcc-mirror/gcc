! { dg-do compile }
! { dg-options "-fdiagnostics-format=json-stderr -fmax-errors=1 -Wfatal-errors" }

program main
    implicit none
    print*, "Hello World!"
end program main

! We expect an empty array as the JSON output.
#if 0
{ dg-begin-multiline-output "" }
[]
{ dg-end-multiline-output "" }
#endif  
