! { dg-do compile }
!
! PR fortran/58803
!
! Contributed by Vittorio Zecca
!
! Was before ICEing due to a double free
!
      type t
       procedure(real), pointer, nopass  :: f1, f2
      end type
      end
