! { dg-do compile }
!
! PR 58998: [4.8/4.9 Regression] Generic interface problem with gfortran
!
! Contributed by Paul van Delst

  interface iargc
    procedure iargc_8
  end interface
  
contains

  integer(8) function iargc_8()
    integer(4) iargc
    iargc_8 = iargc()
  end function
  
end
