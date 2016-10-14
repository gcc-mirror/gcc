! PR middle-end/77959
! { dg-do compile }
! { dg-options "-O2" }

program pr77959
  interface
    subroutine foo(x)  ! { dg-warning "Type mismatch in argument" }
      real :: x
    end
  end interface
  call foo(1.0)
end
subroutine foo(x)
  complex :: x
  x = x + 1
end
