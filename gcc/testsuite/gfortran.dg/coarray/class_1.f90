!{ dg-do compile }
!
! Compiling the call x%f() ICEd.  Check it's fixed.
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>

module pr77872_abs
   type, abstract :: t
   contains
      procedure(s), pass, deferred :: f
   end type
contains
   subroutine s(x)
      class(t) :: x[*]
      call x%f()
   end
end module pr77872_abs
