! { dg-do compile }
! Checks the fix for the bug exposed in fixing PR25147
!
! Contributed by Tobias Schlueter  <tobi@gcc.gnu.org>
!
module integrator
  interface
     function integrate(f,xmin,xmax)
       implicit none
       interface
          function f(x)
            real(8) :: f,x
            intent(in) :: x
          end function f
       end interface
       real(8) :: xmin, xmax, integrate
     end function integrate
  end interface
end module integrator

  use integrator
  call foo1 ()
  call foo2 ()
contains
  subroutine foo1 ()
    real(8) :: f                   ! This was not trapped: PR25147/25098
    print *,integrate (f,0d0,3d0) ! { dg-error "Expected a procedure" }
  end subroutine foo1
  subroutine foo2 ()
    real(8), external :: g ! This would give an error, incorrectly.
    print *,integrate (g,0d0,3d0)
  end subroutine foo2
end
