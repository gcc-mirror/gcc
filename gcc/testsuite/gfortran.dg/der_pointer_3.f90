! { dg-do compile }
! PR 18568
! Find pointer-to-array components
module ints
   type :: bar
      integer, pointer :: th(:)
   end type bar
contains
   function foo(b)
      type(bar), intent(in) :: b
      integer :: foo(size(b%th))
      foo = 0
   end function foo
end module ints

program size_test
  use ints
end program size_test
