! { dg-do run }
! { dg-shouldfail "Recursive" }
  print *, foo_io()
contains
  function foo_io()
    integer :: foo_io(2)
    print * , "foo"
    foo_io = [42, 42]
  end function
end
