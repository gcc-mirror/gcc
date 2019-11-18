! { dg-do compile }
!
! Test the fix for PR69654 in which the derived type 'ty_foo2' was
! not completely built in time for initialization thereby causing an ICE.
!
! Contributed by Hossein Talebi  <talebi.hossein@gmail.com>
!
 Module foo_pointers_class
   implicit none
    type :: ty_foo_pointers
      integer :: scale=0
      integer,pointer :: universe_ulogfile => NULL()
      class(*),pointer :: foo => NULL()
    end type ty_foo_pointers

   type :: ty_part_ptrs
      character(len=80),pointer :: part_name => NULL()
      class(*),pointer     :: part_fem => NULL()
   end type

   type :: ty_class_basis
      integer :: id=0
    end type ty_class_basis

   type :: ty_store_sclass
      class(ty_class_basis),allocatable :: OBJ
   end type ty_store_sclass
End Module foo_pointers_class

Module foo_class
   use foo_pointers_class
   implicit none
   type,extends(ty_class_basis) :: ty_foo2
      character(200)                     :: title
      logical                            :: isInit=.false.
      type(ty_foo_pointers)              :: foo
   end type ty_foo2
ENd Module foo_class


Module foo_scripts_mod
  implicit none
contains

subroutine  foo_script1
   use foo_class, only: ty_foo2
   implicit none
   type(ty_foo2) :: foo2
   integer i

   Call foo_init2(foo2)
end subroutine  foo_script1

subroutine foo_init2(self)
   use foo_class, only: ty_foo2
   type(ty_foo2),target :: self
   self%isInit=.true.
end subroutine foo_init2

End Module foo_scripts_mod
