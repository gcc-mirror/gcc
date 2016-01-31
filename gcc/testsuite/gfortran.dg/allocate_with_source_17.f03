! { dg-do compile }
!
! Tests the fix for PR67564 in which allocate with source for an unlimited
! polymorphic array and a character source would ICE.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
program main
  type :: any_vector
    class(*), allocatable :: x(:)
  end type
  type(any_vector) :: a
  character(kind = 1, len = 5) :: chr1(3) = ["one  ","two  ","three"]
  character(kind = 4, len = 2) :: chr4(2) = [character(kind=4) :: 4_"ab", 4_"cd"]
  real(8) :: r(2) = [1d0,2d0]

  allocate (a%x(3), source = chr1)
  call check
  allocate (a%x(2), source = chr4)
  call check
  allocate (a%x(2), source = r)
  call check

contains
  subroutine check
    select type (z => a%x)
      type is (real(8))
        if (any (z .ne. r)) call abort
      type is (character(kind = 1, len = *))
        if (any(z .ne. chr1)) call abort
      type is (character(kind = 4, len = *))
        if (any(z .ne. chr4)) call abort
    end select
    deallocate (a%x)
  end subroutine
end program
