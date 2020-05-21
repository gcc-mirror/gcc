! { dg-do compile }
!

function g()
 interface
    subroutine g()
    end subroutine g
  end interface
  pointer g
  real g   ! { dg-error "Symbol 'g' at .1. cannot have a type" }
end function

