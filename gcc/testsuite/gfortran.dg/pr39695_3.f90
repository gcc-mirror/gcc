! { dg-do compile }
!

function g()
 interface
    subroutine g()   ! { dg-error "RESULT attribute in 'g'" }
    end subroutine g
  end interface
  real g             ! { dg-error "Symbol 'g' at .1. cannot have a type" }
end function

