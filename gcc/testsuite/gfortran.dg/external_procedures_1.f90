! { dg-do compile }
! { dg-options "-std=f95" }
!
! This tests the patch for PR25024.

! PR25024 - The external attribute for subroutine a would cause an ICE.
  subroutine A ()
    EXTERNAL A  ! { dg-error "EXTERNAL attribute conflicts with SUBROUTINE" }
  END

function ext (y)  ! { dg-error "EXTERNAL attribute conflicts with FUNCTION" }
  real ext, y
  external ext
  !ext = y * y
end function ext

function ext1 (y)
  real ext1, y
  external z        ! OK no conflict
  ext1 = y * y
end function ext1

program main
  real ext, inval
  external ext       ! OK, valid external reference.
  external main      ! { dg-error "PROGRAM attribute conflicts with EXTERNAL" }
  interface
    function ext1 (y)
      real ext1, y
      external ext1
    end function ext1  ! { dg-error "Duplicate EXTERNAL attribute" }
  end interface
  inval = 1.0
  print *, ext(inval)
  print *, ext1(inval)
  print *, inv(inval)
contains
  function inv (y)  ! { dg-error "EXTERNAL attribute conflicts with FUNCTION" }
    real inv, y
    external inv
    !inv = y * y * y
  end function inv
end program main

