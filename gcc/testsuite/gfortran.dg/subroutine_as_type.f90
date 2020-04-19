
subroutine t()
  type t  ! { dg-error "FUNCTION attribute conflicts with SUBROUTINE attribute" }
  end type t ! { dg-error "Expecting END SUBROUTINE statement" }
  type, extends(t) :: t2   ! { dg-error "has not been previously defined" }
  end type t2 ! { dg-error "Expecting END SUBROUTINE statement" }
end
