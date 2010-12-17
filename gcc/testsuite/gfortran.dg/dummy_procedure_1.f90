! { dg-do compile }
! { dg-options "-std=f2003" }
! Test the patch for PR25098, where passing a variable as an
! actual argument to a formal argument that is a procedure
! went undiagnosed.
!
! Based on contribution by Joost VandeVondele  <jv244@cam.ac.uk>
!
integer function y()
  y = 1
end
integer function z()
  z = 1
end

module m1
contains
  subroutine s1(f)
    interface
      function f()
        integer f
      end function f
    end interface
  end subroutine s1
  subroutine s2(x)
    integer :: x
  end subroutine
end module m1

  use m1
  external y
  interface
   function x()
     integer x
   end function x
  end interface

  integer :: i, y, z
  i=1
  call s1(i) ! { dg-error "Expected a procedure for argument" }
  call s1(w) ! { dg-error "used as actual argument" }
  call s1(x) ! explicit interface
  call s1(y) ! declared external
  call s1(z) ! { dg-error "Expected a procedure for argument" }
  call s2(x) ! { dg-error "Invalid procedure argument" }
contains
  integer function w()
    w = 1
  end function w
end

! { dg-final { cleanup-modules "m1" } }
