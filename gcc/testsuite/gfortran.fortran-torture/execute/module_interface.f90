! We were incorrectly mangling procedures in interfaces in modules

module module_interface
    interface
        subroutine foo ()
        end subroutine foo
    end interface
contains
subroutine cs
end subroutine

subroutine cproc
  interface
      subroutine bar ()
      end subroutine
  end interface
  call bar ()
  call foo ()
  call cs ()
end subroutine
end module

subroutine foo ()
end subroutine

subroutine bar ()
end subroutine

program module_interface_proc
  use module_interface
  interface
      subroutine bar ()
      end subroutine
  end interface

  call cproc ()
  call foo ()
  call bar ()
end program
