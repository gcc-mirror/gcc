! { dg-do compile }
! internal compiler error: in gfc_build_dummy_array_decl, at
! fortran/trans-decl.cc:1317
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t0
     integer :: i
   end type
   type t
      class(t0), allocatable :: a(:)
   end type
   class(t0), allocatable :: arg(:)
   allocate (arg, source = [t0(1), t0(2)])
   call s(arg)
contains
   subroutine s(x)
      class(t0) :: x(:)
      type(t) :: z
      associate (y => x)
         z%a = y
      end associate
   if (size(z%a) .ne. 2) stop 1
   end
end
