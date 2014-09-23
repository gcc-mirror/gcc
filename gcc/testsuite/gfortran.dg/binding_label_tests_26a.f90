! { dg-do compile }
!
! PR 58182: [4.9 Regression] ICE with global binding name used as a FUNCTION
!
! Contributed by Andrew Bensons <abensonca@gmail.com>
!

module fg
contains
  function fffi(f)
    interface
       function f() bind(c)
       end function
    end interface
  end function
end module
