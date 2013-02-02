! { dg-do compile }
! PR fortran/50627
module kernels
      select type (args) ! { dg-error "Unexpected SELECT TYPE" }
end module kernels
