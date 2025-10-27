! { dg-do compile }
! { dg-options "-fdump-tree-original" }
module AModule
   implicit none
   private
   public Foo

   interface Foo
      module procedure FooPrivate
   end interface
contains
   subroutine FooPrivate(x)
      integer :: x

      write(*,*) 'Foo(integer)'
   end subroutine
end module
module BModule
   implicit none
   private

   type, public :: BType
   contains
      procedure :: Foo
   end type
contains
   subroutine Foo(self)
      class(BType) :: self

      write(*,*) 'Foo(BType)'
   end subroutine
end module
program iface_tbp_test
   use AModule
   implicit none

   call test()

contains
   subroutine test()
      use BModule

      type(BType) :: y

      call y%Foo()
      call Foo(1)
   end subroutine
end program
! { dg-final { scan-tree-dump-times "foo \\(&class.2\\)" 1 "original" } }
