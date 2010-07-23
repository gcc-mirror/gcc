! { dg-do compile }
! 
! gfortran was ICEing for the constructor of
! componentfree types.
!
! Contributed by James Van Buskirk
! http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/c8dd08d6da052499/
!
 module bug4_mod
   implicit none
   type bug4 ! no components
   end type bug4
end module bug4_mod

program bug4_structure
   use bug4_mod
   implicit none
   type(bug4) t
   t = bug4()
   write(*,*) t
end program bug4_structure
! { dg-final { cleanup-modules "bug4_mod" } }
