! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! PR fortran/37336
!
module m
  implicit none
  type t
    integer :: i
  contains
    final :: fini, fini2
  end type t
  integer :: global_count1, global_count2
contains
  subroutine fini(x)
    type(t) :: x
    !print *, 'fini:',x%i
    if (global_count1 == -1) STOP 1
    if (x%i /= 42) STOP 2 
    x%i = 33
    global_count1 = global_count1 + 1
  end subroutine fini
  subroutine fini2(x)
    type(t) :: x(:)
    !print *, 'fini2', x%i
    if (global_count2 == -1) STOP 3
    if (size(x) /= 5) STOP 4
    if (any (x%i /= [1,2,3,4,5]) .and. any (x%i /= [6,7,8,9,10])) STOP 5 
    x%i = 33
    global_count2 = global_count2 + 10
  end subroutine fini2
end module m

program pp
  use m
  implicit none
  type(t), allocatable :: ya
  class(t), allocatable :: yc
  type(t), allocatable :: yaa(:)
  class(t), allocatable :: yca(:)

  type(t), allocatable :: ca[:]
  class(t), allocatable :: cc[:]
  type(t), allocatable :: caa(:)[:]
  class(t), allocatable :: cca(:)[:]

  global_count1 = -1
  global_count2 = -1
  allocate (ya, yc, yaa(5), yca(5))
  global_count1 = 0
  global_count2 = 0
  ya%i = 42
  yc%i = 42
  yaa%i = [1,2,3,4,5]
  yca%i = [1,2,3,4,5]

  call foo(ya, yc, yaa, yca)
  if (global_count1 /= 2) STOP 6
  if (global_count2 /= 20) STOP 7

  ! Coarray finalization
  allocate (ca[*], cc[*], caa(5)[*], cca(5)[*])
  global_count1 = 0
  global_count2 = 0
  ca%i = 42
  cc%i = 42
  caa%i = [1,2,3,4,5]
  cca%i = [1,2,3,4,5]
  deallocate (ca, cc, caa, cca)
  if (global_count1 /= 2) STOP 8
  if (global_count2 /= 20) STOP 9
  global_count1 = -1
  global_count2 = -1

  block
    type(t), allocatable :: za
    class(t), allocatable :: zc
    type(t), allocatable :: zaa(:)
    class(t), allocatable :: zca(:)

    ! Test intent(out) finalization
    allocate (za, zc, zaa(5), zca(5))
    global_count1 = 0
    global_count2 = 0
    za%i = 42
    zc%i = 42
    zaa%i = [1,2,3,4,5]
    zca%i = [1,2,3,4,5]

    call foo(za, zc, zaa, zca)
    if (global_count1 /= 2) STOP 10
    if (global_count2 /= 20) STOP 11

    ! Test intent(out) finalization with optional
    call foo_opt()
    call opt()

    ! Test intent(out) finalization with optional
    allocate (za, zc, zaa(5), zca(5))
    global_count1 = 0
    global_count2 = 0
    za%i = 42
    zc%i = 42
    zaa%i = [1,2,3,4,5]
    zca%i = [1,2,3,4,5]

    call foo_opt(za, zc, zaa, zca)
    if (global_count1 /= 2) STOP 12
    if (global_count2 /= 20) STOP 13

    ! Test DEALLOCATE finalization
    allocate (za, zc, zaa(5), zca(5))
    global_count1 = 0
    global_count2 = 0
    za%i = 42
    zc%i = 42
    zaa%i = [1,2,3,4,5]
    zca%i = [6,7,8,9,10]
    deallocate (za, zc, zaa, zca)
    if (global_count1 /= 2) STOP 14
    if (global_count2 /= 20) STOP 15

    ! Test end-of-scope finalization
    allocate (za, zc, zaa(5), zca(5))
    global_count1 = 0
    global_count2 = 0
    za%i = 42
    zc%i = 42
    zaa%i = [1,2,3,4,5]
    zca%i = [6,7,8,9,10]
  end block

  if (global_count1 /= 2) STOP 16
  if (global_count2 /= 20) STOP 17

  ! Test that no end-of-scope finalization occurs
  ! for SAVED variable in main
  allocate (ya, yc, yaa(5), yca(5))
  global_count1 = -1
  global_count2 = -1

contains

  subroutine opt(xa, xc, xaa, xca)
    type(t),  allocatable, optional :: xa
    class(t), allocatable, optional :: xc
    type(t),  allocatable, optional :: xaa(:)
    class(t), allocatable, optional :: xca(:)
    call foo_opt(xc, xc, xaa)
    !call foo_opt(xa, xc, xaa, xca) ! FIXME: Fails (ICE) due to PR 57445
  end subroutine opt
  subroutine foo_opt(xa, xc, xaa, xca)
    type(t),  allocatable, intent(out), optional :: xa
    class(t), allocatable, intent(out), optional :: xc
    type(t),  allocatable, intent(out), optional :: xaa(:)
    class(t), allocatable, intent(out), optional :: xca(:)

    if (.not. present(xa)) &
      return
    if (allocated (xa)) STOP 18
    if (allocated (xc)) STOP 19
    if (allocated (xaa)) STOP 20
    if (allocated (xca)) STOP 21
  end subroutine foo_opt
  subroutine foo(xa, xc, xaa, xca)
    type(t),  allocatable, intent(out) :: xa
    class(t), allocatable, intent(out) :: xc
    type(t),  allocatable, intent(out) :: xaa(:)
    class(t), allocatable, intent(out) :: xca(:)
    if (allocated (xa)) STOP 22
    if (allocated (xc)) STOP 23
    if (allocated (xaa)) STOP 24
    if (allocated (xca)) STOP 25
  end subroutine foo
end program
