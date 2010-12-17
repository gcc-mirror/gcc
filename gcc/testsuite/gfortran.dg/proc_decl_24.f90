! { dg-do compile }
!
! Contributed by James van Buskirk
!
! http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/44d572766bce0e6f/

  use iso_c_binding
  implicit none

  abstract interface
    subroutine all_subs(x,y) bind(C)
      use iso_c_binding
      real(c_float) :: x,y
    end subroutine all_subs
  end  interface

  procedure(all_subs) :: sub
  type(C_FUNPTR) :: s

  s = c_funloc (sub)

end
