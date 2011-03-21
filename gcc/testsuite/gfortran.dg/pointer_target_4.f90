! { dg-do compile }
!
! PR fortran/47377
!
! Contributed by <thenlich@users.sourceforge.net>
!
program testgferr
    real, pointer :: y
    y => f()  ! { dg-error "must deliver a pointer result" }
contains
    function f()
      real :: f
      f = 5
    end function f
end program testgferr
