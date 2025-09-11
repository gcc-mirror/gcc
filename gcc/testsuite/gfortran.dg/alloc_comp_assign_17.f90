! { dg-do run }
! PR fortran/121616
!
! Test fix for intrinsic assignment to allocatable scalar polymorphic component

program p
  call pr121616 ()
  call test_ts  ()
end

! Derived from original PR (contributed by Jean Vézina)
subroutine pr121616 ()
  implicit none
  integer :: i
  type general
     class(*), allocatable :: x
  end type general
  type(general) :: a(4), b(4)
  ! Intrinsic assignment to a variable of unlimited polymorphic type
  a(1)%x = 1
  a(2)%x = 3.14
  a(3)%x = .true.
  a(4)%x = 'abc'
  ! The workaround was to use a structure constructor
  b(1) = general(1)
  b(2) = general(3.14)
  b(3) = general(.true.)
  b(4) = general('abc') 
  do i = 1, 4
     if (.not. allocated (a(i)%x)) stop 10+i
     if (.not. allocated (b(i)%x)) stop 20+i
     call prt (a(i)%x, b(i)%x)
  end do
  do i = 1, 4
     deallocate (a(i)%x, b(i)%x)
  end do
contains
  subroutine prt (x, y)
    class(*), intent(in) :: x, y
    select type (v=>x)
    type is (integer)
       print *,v
    type is (real)
       print *,v
    type is (logical)
       print *,v
    type is (character(*))
       print *,v
    class default
       error stop 99
    end select
    if (.not. same_type_as (x, y)) stop 30+i
  end subroutine prt
end

! Contributed by a friend (private communication)
subroutine test_ts ()
  implicit none

  type :: t_inner
    integer :: i
  end type

  type :: t_outer
    class(t_inner), allocatable :: inner
  end type

  class(t_inner), allocatable :: inner
  type(t_outer),  allocatable :: outer(:)
  integer :: i

  allocate(t_inner :: inner)
  inner% i = 0

  !------------------------------------------------
  ! Size of outer must be > 1 for the bug to appear
  !------------------------------------------------
  allocate(outer(2))

  !------------------------------
  ! Loop is necessary for the bug
  !------------------------------
  do i = 1, size(outer)
    write(*,*) i
    !----------------------------------------------------
    ! Expect intrinsic assignment to polymorphic variable
    !----------------------------------------------------
    outer(i)% inner = inner
    deallocate (outer(i)% inner)
  end do

  write(*,*) 'Loop DONE'
  deallocate(outer)
  deallocate(inner)
  write(*,*) 'Dellocation DONE'
end
