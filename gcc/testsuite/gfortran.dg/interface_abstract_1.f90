! { dg-do compile }
!
module mod_interf_abstract
implicit none
abstract interface :: one ! { dg-error "Syntax error in ABSTRACT INTERFACE statement" }
end interface ! { dg-error "Expecting END MODULE statement" }

abstract interface
  subroutine two() bind(C)
  end subroutine two
  subroutine three() bind(C,name="three") ! { dg-error "NAME not allowed on BIND.C. for ABSTRACT INTERFACE" }
  end subroutine three ! { dg-error "Expecting END INTERFACE statement" }
  subroutine real() ! { dg-error "cannot be the same as an intrinsic type" }
  end subroutine real
end interface

contains

  subroutine sub() bind(C,name="subC")
  end subroutine

end module mod_interf_abstract
