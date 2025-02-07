! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/59252

module mod
  implicit none

  type t1
     character(256), allocatable :: label
  end type t1

  type t2
     type(t1),       allocatable :: appv(:)
  end type t2

contains
  subroutine construct(res)
    type(t2), allocatable, intent(inout) :: res
    if (.not. allocated(res)) allocate(res)
  end subroutine construct

  subroutine construct_appv(appv)
    type(t1), allocatable, intent(inout) :: appv(:)
    if (.not. allocated(appv)) allocate(appv(20))
  end subroutine construct_appv

  type(t1) function foo () result (res)
  end function foo
end module mod

program testy
  use mod
  implicit none
  type(t2), allocatable :: res
  type(t1)              :: s

  ! original test from pr59252
  call construct     (res)
  call construct_appv(res%appv)
  deallocate (res)

  ! related test from pr118747 comment 2:
  s = foo ()
end program testy

! { dg-final { scan-tree-dump-not "__builtin_memmove" "original" } }
