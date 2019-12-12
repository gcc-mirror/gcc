! { dg-do compile }
! PR fortran/50627
module kernels
      select type (args) ! { dg-error "cannot appear in this scope" }
end module kernels
