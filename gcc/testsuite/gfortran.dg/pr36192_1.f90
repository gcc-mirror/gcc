! { dg-do compile }
! PR fortran/36192
program three_body
   real, parameter ::  n = 2, d = 2
   real, dimension(n,d) :: x_hq ! { dg-error "Expecting a scalar INTEGER" }
   call step(x_hq)
   contains
   subroutine step(x)
      real, dimension(:,:), intent(in) :: x
   end subroutine step
end program three_body
! { dg-prune-output "Rank mismatch in argument" }
