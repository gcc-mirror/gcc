! { dg-do compile }
!
! PR 58182: [4.9 Regression] ICE with global binding name used as a FUNCTION
!
! Contributed by Andrew Bensons <abensonca@gmail.com>
!
! This file must be compiled BEFORE binding_label_tests_26b.f90, which it 
! should be because dejagnu will sort the files.  

module fg
contains
  function fffi(f)
    interface
       function f() bind(c)
       end function
    end interface
  end function
end module

! { dg-final { keep-modules "" } }
