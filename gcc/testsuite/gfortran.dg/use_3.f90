! { dg-do compile }
module foo
end module foo

  use foo
  use :: foo
  use, intrinsic iso_fortran_env ! { dg-error "\"::\" was expected after module nature" }
  use, non_intrinsic iso_fortran_env ! { dg-error "\"::\" was expected after module nature" }
  use, nonintrinsic :: iso_fortran_env ! { dg-error "shall be either INTRINSIC or NON_INTRINSIC" }
  use, intrinsic :: iso_fortran_env
end
! { dg-final { cleanup-modules "foo" } }
