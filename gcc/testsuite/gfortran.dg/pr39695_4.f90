! { dg-do compile }
!

function g()
  implicit none
  interface
    function g()
      integer g
    end function g
  end interface
  pointer g
  real g   ! { dg-error "Symbol 'g' at .1. already has basic type of INTEGER" }
end function

