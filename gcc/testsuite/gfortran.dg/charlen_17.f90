! { dg-do compile }
! PR 87673 - used to cause errors about non-pure functions.

module x
  implicit none
contains
  pure function foo() result(res)
    character(len=:), allocatable :: res
    allocate (character(bar()) :: res)
  end function foo
  pure integer function bar()
    bar = 1
  end function bar
end module x
