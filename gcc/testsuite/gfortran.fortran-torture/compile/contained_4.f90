! Check contained functions with the same name.
module contained_4

contains

  subroutine foo1()
    call bar()
  contains
    subroutine bar()
    end subroutine bar
  end subroutine foo1

  subroutine foo2()
    call bar()
  contains
    subroutine bar()
    end subroutine bar
  end subroutine foo2

end module contained_4

subroutine foo1()
call bar()
contains
  subroutine bar()
  end subroutine bar
end subroutine

subroutine foo2()
  call bar()
contains
  subroutine bar()
  end subroutine bar
end subroutine foo2

