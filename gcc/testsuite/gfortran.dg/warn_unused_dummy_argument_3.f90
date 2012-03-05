! { dg-do compile }
! { dg-options "-Wunused-dummy-argument -Wunused-parameter" }
! PR 48847 - we used to generate a warning for g(), and none for h()
program main
contains
  function f(g,h)
    interface
       real function g()
       end function g
    end interface
    interface
       real function h()  !  { dg-warning "Unused dummy argument" }
       end function h
    end interface
    real :: f
    f = g()
  end function f
end program main
