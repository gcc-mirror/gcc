! { dg-do compile }
!
! This tests the fix for PR36361: If a function was declared in an INTERFACE
! statement, no attributes may be declared outside of the INTERFACE body.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m1
  interface
    real function f1()
    end function
  end interface
  dimension :: f1(4)  ! { dg-error "outside its INTERFACE body" }
end module


module m2
  dimension :: f2(4)
  interface
    real function f2()  ! { dg-error "outside its INTERFACE body" }
    !end function
  end interface
end module


! valid
module m3
  interface
    real function f3()
      dimension :: f3(4)
    end function
  end interface
end module


module m4
  interface
    function f4()  ! { dg-error "cannot have a deferred shape" }
      real :: f4(:)
    end function
  end interface
  allocatable :: f4  ! { dg-error "outside of INTERFACE body" }
end module


module m5
  allocatable :: f5(:)
  interface
    function f5()  ! { dg-error "outside its INTERFACE body" }
      !real f5(:)
    !end function
  end interface
end module


!valid
module m6
  interface
    function f6()
      real f6(:)
      allocatable :: f6
    end function
  end interface
end module

! { dg-final { cleanup-modules "m1 m2 m3 m4 m5 m6" } }
