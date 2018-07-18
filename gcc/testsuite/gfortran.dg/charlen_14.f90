! { dg-do compile }
! PR fortran/69064
subroutine setup_check_path(path)             ! { dg-error "has no IMPLICIT type" }
  implicit none
  character(len=path_len),intent(inout)::path ! { dg-error "Scalar INTEGER expression" }
end
