! { dg-do compile }
! PR fortran/89943
! Code contributed by Alberto Luaces  <aluaces at udc dot se>
module Foo_mod

   implicit none

   interface
      module function runFoo4C(ndim) bind(C, name="runFoo")
         use, intrinsic :: iso_c_binding
         implicit none
         integer runFoo4c
         integer(c_int32_t) , intent(in) :: ndim
      end function runFoo4C
   end interface

   contains

end module Foo_mod

submodule(Foo_mod) Foo_smod

   contains

      module function runFoo4C(ndim) bind(C, name="runFoo")
         use, intrinsic :: iso_c_binding
         implicit none
         integer runFoo4c
         integer(c_int32_t) , intent(in) :: ndim
      end function runFoo4C

end submodule Foo_smod

