! { dg-do run }
! { dg-skip-if "" { powerpc-ibm-aix* } }
!
! PR 56500: [OOP] "IMPLICIT CLASS(...)" wrongly rejected
!
! Contributed by Reinhold Bader <Reinhold.Bader@lrz.de>

! Add dump-fortran-original to check, if the patch preventing a gfortran
! segfault is working correctly.  No cleanup needed, because the dump
! goes to stdout.
! { dg-options "-fdump-fortran-original" }
! { dg-allow-blank-lines-in-output 1 }
! { dg-prune-output "Namespace:.*-{42}" }

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
    if (aaf%i /= 2) STOP 1
  class default
    STOP 2
  end select

  allocate(caf, source=foo(3))
  select type (caf)
  type is (foo)
    if (caf%i /= 3) STOP 3
  class default
    STOP 4
  end select

contains
  subroutine gloo(x)
    implicit class(*) (a-z)
  end
end program
