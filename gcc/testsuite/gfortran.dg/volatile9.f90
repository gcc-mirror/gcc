! Check for valid VOLATILE uses
!
! Contributed by Steven Correll.
!
! PR fortran/30520

! { dg-do compile }
 
  function f() result(fr)
    integer, volatile :: fr
    fr = 5
  end function f 

  module mod13
    implicit none
    integer :: v13
  end module mod13 

  module mod13a
   use mod13
   implicit none
   volatile :: v13
   real :: v14
  contains
   subroutine s13()
     volatile :: v13
     volatile :: v14
   end subroutine s13 
  end module mod13a 

  module mod13b
   use mod13a
   implicit none
   volatile :: v13
  end module mod13b 


  subroutine s14()
    use mod13a
    implicit none
    volatile :: v13
  end subroutine s14 

! { dg-final { cleanup-modules "mod13 mod13a mod13b" } }
