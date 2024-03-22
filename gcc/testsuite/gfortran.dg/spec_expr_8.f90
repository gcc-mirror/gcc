! { dg-do compile }
!
! PR fortran/111781
! We used to reject the example below because the dummy procedure g was
! setting the current namespace without properly restoring it, which broke
! the specification expression check for the dimension of A later on.
!
! Contributed by Rasmus Vikhamar-Sandberg  <rasmus.vikhamar-sandberg@uit.no>

program example
    implicit none
    integer :: n
    
contains
    
    subroutine f(g,A)
        real, intent(out) :: A(n)
        interface
          pure real(8) function g(x)
            real(8), intent(in) :: x
          end function
        end interface
    end subroutine
end program
