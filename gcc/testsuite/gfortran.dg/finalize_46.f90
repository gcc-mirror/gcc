! { dg-do run }
!
! Test the fix for pr88735.
!
! Contributed by Martin Stein  <mscfd@gmx.net>
!
module mod
  implicit none
  type, public :: t
     integer, pointer :: i => NULL ()
     character :: myname = 'z'
     character :: alloc = 'n'
  contains
     procedure, public :: set
     generic, public :: assignment(=) => set
     final :: finalise
  end type t
  integer, public :: assoc_in_final = 0
  integer, public :: calls_to_final = 0
  character, public :: myname1, myname2

contains

  subroutine set(self, x)
     class(t), intent(out) :: self
     class(t), intent(in)  :: x
     if (associated(self%i)) then
        stop 1                               ! Default init for INTENT(OUT)
     endif
     if (associated(x%i)) then
        myname2 = self%myname
        self%i => x%i
        self%i = self%i + 1
     end if
end subroutine set

  subroutine finalise(self)
     type(t), intent(inout) :: self
     calls_to_final = calls_to_final + 1
     myname1 = self%myname
     if (associated(self%i)) then
        assoc_in_final = assoc_in_final + 1
        if (self%alloc .eq. 'y') deallocate (self%i)
     end if
  end subroutine finalise

end module mod

program finalise_assign
  use mod
  implicit none
  type :: s
     integer :: i = 0
     type(t) :: x
  end type s
  type(s) :: a, b
  type(t) :: c
  a%x%myname = 'a'
  b%x%myname = 'b'
  c%myname = 'c'
  allocate (a%x%i)
  a%x%i = 123
  a%x%alloc = 'y'

  b = a
  if (assoc_in_final /= 0) stop 2  ! b%x%i not associated before finalization
  if (calls_to_final /= 2) stop 3  ! One finalization call
  if (myname1 .ne. 'b') stop 4     ! Finalization before intent out become undefined
  if (myname2 .ne. 'z') stop 5     ! Intent out now default initialized
  if (.not.associated (b%x%i, a%x%i)) stop 6

  allocate (c%i, source = 789)
  c%alloc = 'y'
  c = a%x
  if (assoc_in_final /= 1) stop 6  ! c%i is allocated prior to the assignment
  if (calls_to_final /= 3) stop 7  ! One finalization call for the assignment
  if (myname1 .ne. 'c') stop 8     ! Finalization before intent out become undefined
  if (myname2 .ne. 'z') stop 9     ! Intent out now default initialized

  b = a
  if (assoc_in_final /= 3) stop 10 ! b%i is associated by earlier assignment
  if (calls_to_final /= 5) stop 11 ! One finalization call for the assignment
  if (myname1 .ne. 'z') stop 12    ! b%x%myname was default initialized in earlier assignment
  if (myname2 .ne. 'z') stop 13    ! Intent out now default initialized
  if (b%x%i .ne. 126) stop 14      ! Three assignments with self%x%i pointing to same target
  deallocate (a%x%i)
  if (.not.associated (b%x%i, c%i)) then
    stop 15                        ! ditto
    b%x%i =>NULL ()                ! Although not needed here, clean up
    c%i => NULL ()
  endif
end program finalise_assign
