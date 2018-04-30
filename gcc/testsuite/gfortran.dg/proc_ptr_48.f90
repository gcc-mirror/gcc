! { dg-do run }
!
! Checks the fix for PR68196, comment #8
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  type  Bug                                  ! Failed at trans--array.c:8269
    real, allocatable :: scalar
    procedure(boogInterface),pointer :: boog
  end type
  interface
    function boogInterface(A) result(C)
      import Bug
      class(Bug) A
      type(Bug)  C
    end function
  end interface

  real, parameter :: ninetynine = 99.0
  real, parameter :: onenineeight = 198.0

  type(bug) :: actual, res

  actual%scalar = ninetynine
  actual%boog => boogImplementation

  res = actual%boog ()                       ! Failed on bug in expr.c:3933
  if (res%scalar .ne. onenineeight) STOP 1

! Make sure that the procedure pointer is assigned correctly
  if (actual%scalar .ne. ninetynine) STOP 2
  actual = res%boog ()
  if (actual%scalar .ne. onenineeight) STOP 3

! Deallocate so that we can use valgrind to check for memory leaks
  deallocate (res%scalar, actual%scalar)

contains
    function boogImplementation(A) result(C) ! Failed at trans--array.c:8078
      class(Bug) A
      type(Bug)  C
      select type (A)
        type is (bug)
          C = A
          C%scalar = onenineeight
        class default
          STOP 4
      end select
    end function
end
