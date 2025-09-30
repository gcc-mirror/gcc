! { dg-do run }
!
! Contributed by FortranFan at https://groups.google.com/g/comp.lang.fortran/c/NDE6JKTFbNU
!
   integer, parameter :: parm = 42
   type :: t(ell)
      integer, len :: ell
      integer :: i
   end type

   type :: u
      type(t(ell=:)), allocatable :: x
   end type

   type(t(ell=:)), allocatable :: foo
   type(u) :: bar

   allocate( t(ell = parm) :: foo )
   foo%i = 2 * foo%ell

   bar = u (foo)                    ! Gave: Cannot convert TYPE(Pdtt) to TYPE(t)

   if (bar%x%ell /= parm) stop 1    ! Then these component references failed in
   if (bar%x%i /= 2 * parm) stop 2  ! translation.
   deallocate (foo, bar%x)
end
