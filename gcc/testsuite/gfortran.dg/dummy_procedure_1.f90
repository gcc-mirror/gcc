! { dg-do compile }
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
  call s1(w) ! { dg-error "not allowed as an actual argument" }
  call s1(x) ! explicit interface
  call s1(y) ! declared external
  call s1(z) ! already compiled
contains
  integer function w()
    w = 1
  end function w
end

! { dg-final { cleanup-modules "m1" } }
