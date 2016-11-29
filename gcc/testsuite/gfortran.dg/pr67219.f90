! { dg-do compile }
! PR 67149 - this used to throw a spurious error.
function foo(bar)
   integer(8)             :: foo
   integer(4), intent(in) :: bar
   integer(4), parameter  :: huge_4 = huge(0_4)
   foo = (huge_4 - int(bar,kind=8))
end function
