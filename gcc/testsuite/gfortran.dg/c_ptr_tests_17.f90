! { dg-do compile }
!
! PR fortran/37829
!
! Contributed by James Van Buskirk and Jerry DeLisle.
!
! Fix derived-type loading with ISO_BIND_C's C_PTR/C_FUNPTR.

module m3
   use ISO_C_BINDING
   implicit none
   private

   public kill_C_PTR
   interface
      function kill_C_PTR() bind(C)
         import
         implicit none
         type(C_PTR) kill_C_PTR
      end function kill_C_PTR
   end interface

   public kill_C_FUNPTR
   interface
      function kill_C_FUNPTR() bind(C)
         import
         implicit none
         type(C_FUNPTR) kill_C_FUNPTR
      end function kill_C_FUNPTR
   end interface
end module m3

module m1
   use m3
end module m1

program X
   use m1
   use ISO_C_BINDING
   implicit none
   type(C_PTR) cp
   type(C_FUNPTR) fp
   integer(C_INT),target :: i
   interface
      function fun() bind(C)
         use ISO_C_BINDING
         implicit none
         real(C_FLOAT) fun
      end function fun
   end interface

   cp = C_NULL_PTR
   cp = C_LOC(i)
   fp = C_NULL_FUNPTR
   fp = C_FUNLOC(fun)
end program X

function fun() bind(C)
   use ISO_C_BINDING
   implicit none
   real(C_FLOAT) fun
   fun = 1.0
end function fun

function kill_C_PTR() bind(C)
   use ISO_C_BINDING
   implicit none
   type(C_PTR) kill_C_PTR
   integer(C_INT), pointer :: p
   allocate(p)
   kill_C_PTR = C_LOC(p)
end function kill_C_PTR

function kill_C_FUNPTR() bind(C)
   use ISO_C_BINDING
   implicit none
   type(C_FUNPTR) kill_C_FUNPTR
   interface
      function fun() bind(C)
         use ISO_C_BINDING
         implicit none
         real(C_FLOAT) fun
      end function fun
   end interface
   kill_C_FUNPTR = C_FUNLOC(fun)
end function kill_C_FUNPTR
