! { dg-do compile }
! PR fortran/103139
! Contributed by G. Steinmetz <gscfq@t-online.de>
!
! ICE in fold_convert_loc with structure constructor containing
! unlimited polymorphic allocatable component.

module m
  type :: t1
    class(*), allocatable :: c
  end type
contains
  subroutine s1 ()
    type(t1) :: a
    a = t1 (42)
  end subroutine
end module
