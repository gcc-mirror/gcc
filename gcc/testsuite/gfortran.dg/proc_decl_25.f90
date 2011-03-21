! { dg-do compile }
!
! PR 47352: [F03] ICE with proc-pointers in generic procedures
!
! Contributed by James van Buskirk
! cf. http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/bbaf59ffd7c372e9

   implicit none

   abstract interface
      real function f()
      end function f
   end interface

   procedure(f) :: f1

   interface gen
      procedure f1
   end interface gen

   write(*,*) gen()
end
