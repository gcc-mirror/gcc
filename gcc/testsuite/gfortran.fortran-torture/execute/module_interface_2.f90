! Test generic interfaces declared in modules.
! We used to get the name mangling wrong for these.
module module_interface_2
  interface foo
    subroutine myfoo (i)
      integer i
    end subroutine
    module procedure bar
  end interface
contains
subroutine bar (r)
  real r

  if (r .ne. 1.0) STOP 1
end subroutine
end module

subroutine myfoo (i)
  integer i

  if (i .ne. 42) STOP 2
end subroutine

program test
  use module_interface_2

  call foo (42)
  call foo (1.0)
end program
