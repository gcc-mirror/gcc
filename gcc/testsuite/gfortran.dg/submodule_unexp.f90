! { dg-do compile }
! PR fortran/69498
! This used to ICE
program p
   type t
   submodule (m) sm ! { dg-error "SUBMODULE declaration at" }
   end type
end
