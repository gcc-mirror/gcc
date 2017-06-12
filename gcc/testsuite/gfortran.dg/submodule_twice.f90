! { dg-do compile }
! PR fortran/69498
! This used to ICE
program main
    submodule (m) sm ! { dg-error "SUBMODULE declaration at" }
    submodule (m2) sm2  ! { dg-error "SUBMODULE declaration at" }
end program
