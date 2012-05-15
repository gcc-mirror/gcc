! { dg-do compile }
module iso_fortran_env
end module iso_fortran_env

program foo
 use, intrinsic :: iso_fortran_env
 use, non_intrinsic :: iso_fortran_env ! { dg-error "conflicts with intrinsic module" }
end program foo

subroutine truc
 use, non_intrinsic :: iso_fortran_env
 use, intrinsic :: iso_fortran_env ! { dg-error "conflicts with non-intrinsic module" }
end subroutine truc
