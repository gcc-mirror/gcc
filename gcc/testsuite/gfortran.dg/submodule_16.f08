! { dg-do compile }
!
! Tests the fix for PR71156 in which the valid code (f7, f8 and f9 below)
! triggered an error, while the invalid code (f1 to f6) compiled.
!
! Contributed by Damian Rousn  <damian@sourceryinstitute.org>
!
module my_interface
  implicit none
  interface
    module subroutine f1
    end subroutine
    module subroutine f2
    end subroutine
    module subroutine f3
    end subroutine
    elemental module subroutine f4
    end subroutine
    pure module subroutine f5
    end subroutine
    recursive module subroutine f6
    end subroutine
    elemental module subroutine f7
    end subroutine
    pure module subroutine f8
    end subroutine
    recursive module subroutine f9
    end subroutine
  end interface
end module

submodule(my_interface) my_implementation
  implicit none
contains
    elemental module subroutine f1 ! { dg-error "Mismatch in ELEMENTAL attribute" }
    end subroutine
    pure module subroutine f2 ! { dg-error "Mismatch in PURE attribute" }
    end subroutine
    recursive module subroutine f3 ! { dg-error "Mismatch in RECURSIVE attribute" }
    end subroutine
    module subroutine f4 ! { dg-error "ELEMENTAL prefix" }
    end subroutine
    module subroutine f5 ! { dg-error "PURE prefix" }
    end subroutine
    module subroutine f6 ! { dg-error "RECURSIVE prefix" }
    end subroutine
    elemental module subroutine f7
    end subroutine
    pure module subroutine f8
    end subroutine
    recursive module subroutine f9
    end subroutine
end submodule
