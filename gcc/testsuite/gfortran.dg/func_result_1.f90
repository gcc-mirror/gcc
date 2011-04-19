! { dg-do run }
! From PR 19673 : We didn't dereference the result from POINTER
! functions with a RESULT clause
program ret_ptr
  if (foo(99) /= bar(99)) call abort ()
contains
  function foo (arg) result(ptr)
    integer :: arg
    integer, pointer :: ptr
    allocate (ptr)
    ptr = arg
  end function foo
  function bar (arg)
    integer :: arg
    integer, pointer :: bar
    allocate (bar)
    bar = arg
  end function bar
end  program ret_ptr
