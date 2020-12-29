! { dg-do compile }
!
! Test the fix for PR97612.
!
! Contributed by Martin Stein  <mscfd@gmx.net>
!
program constructor_allocatable
  implicit none

  type :: s
    integer, dimension(:), allocatable :: u
  end type s

  type :: t
    type(s), dimension(:), allocatable :: x
  end type t

  type(t) :: a = t()
  if (allocated (a%x)) stop 1

end program constructor_allocatable
