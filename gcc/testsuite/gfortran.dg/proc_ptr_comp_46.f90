! { dg-do compile }
!
! PR 77596: [F03] procedure pointer component with implicit interface can point to a function
!
! Contributed by toK <t.kondic@leeds.ac.uk>

program xxx
  implicit none

  type tf
     procedure(), nopass, pointer :: fp
  end type tf

  call ass()

contains

  integer function ff(x)
    integer, intent(in) :: x
    ff = x + 1
  end function ff

  subroutine ass()
    type(tf) :: p
    p%fp=>ff        ! { dg-error "is not a subroutine" }
    call p%fp(3)
  end subroutine ass

end
