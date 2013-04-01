! { dg-do run }
!
! PR 56500: [OOP] "IMPLICIT CLASS(...)" wrongly rejected
!
! Contributed by Reinhold Bader <Reinhold.Bader@lrz.de>

program upimp
  implicit class(foo) (a-b)
  implicit class(*) (c)
  type :: foo
    integer :: i
  end type
  allocatable :: aaf, caf

  allocate(aaf, source=foo(2))
  select type (aaf)
  type is (foo)
    if (aaf%i /= 2) call abort
  class default
    call abort
  end select

  allocate(caf, source=foo(3))
  select type (caf)
  type is (foo)
    if (caf%i /= 3) call abort
  class default
    call abort
  end select

contains
  subroutine gloo(x)
    implicit class(*) (a-z)
  end
end program
