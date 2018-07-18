! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! PR fortran/50981
! PR fortran/54618
!

  implicit none
  type t
   integer, allocatable :: i
  end type t
  type, extends (t):: t2
   integer, allocatable :: j
  end type t2

  class(t), allocatable :: xa, xa2(:), xac[:], xa2c(:)[:]
  class(t), pointer :: xp, xp2(:)

  xp => null()
  xp2 => null()

  call suba(alloc=.false., prsnt=.false.)
  call suba(xa, alloc=.false., prsnt=.true.)
  if (.not. allocated (xa)) STOP 1
  if (.not. allocated (xa%i)) STOP 2
  if (xa%i /= 5) STOP 3
  xa%i = -3
  call suba(xa, alloc=.true., prsnt=.true.)
  if (allocated (xa)) STOP 4

  call suba2(alloc=.false., prsnt=.false.)
  call suba2(xa2, alloc=.false., prsnt=.true.)
  if (.not. allocated (xa2)) STOP 5
  if (size (xa2) /= 1) STOP 6
  if (.not. allocated (xa2(1)%i)) STOP 7
  if (xa2(1)%i /= 5) STOP 8
  xa2(1)%i = -3
  call suba2(xa2, alloc=.true., prsnt=.true.)
  if (allocated (xa2)) STOP 9

  call subp(alloc=.false., prsnt=.false.)
  call subp(xp, alloc=.false., prsnt=.true.)
  if (.not. associated (xp)) STOP 10
  if (.not. allocated (xp%i)) STOP 11
  if (xp%i /= 5) STOP 12
  xp%i = -3
  call subp(xp, alloc=.true., prsnt=.true.)
  if (associated (xp)) STOP 13

  call subp2(alloc=.false., prsnt=.false.)
  call subp2(xp2, alloc=.false., prsnt=.true.)
  if (.not. associated (xp2)) STOP 14
  if (size (xp2) /= 1) STOP 15
  if (.not. allocated (xp2(1)%i)) STOP 16
  if (xp2(1)%i /= 5) STOP 17
  xp2(1)%i = -3
  call subp2(xp2, alloc=.true., prsnt=.true.)
  if (associated (xp2)) STOP 18

  call subac(alloc=.false., prsnt=.false.)
  call subac(xac, alloc=.false., prsnt=.true.)
  if (.not. allocated (xac)) STOP 19
  if (.not. allocated (xac%i)) STOP 20
  if (xac%i /= 5) STOP 21
  xac%i = -3
  call subac(xac, alloc=.true., prsnt=.true.)
  if (allocated (xac)) STOP 22

  call suba2c(alloc=.false., prsnt=.false.)
  call suba2c(xa2c, alloc=.false., prsnt=.true.)
  if (.not. allocated (xa2c)) STOP 23
  if (size (xa2c) /= 1) STOP 24
  if (.not. allocated (xa2c(1)%i)) STOP 25
  if (xa2c(1)%i /= 5) STOP 26
  xa2c(1)%i = -3
  call suba2c(xa2c, alloc=.true., prsnt=.true.)
  if (allocated (xa2c)) STOP 27

contains
 subroutine suba2c(x, prsnt, alloc)
   class(t), optional, allocatable :: x(:)[:]
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) STOP 28
   if (prsnt) then
     if (alloc .neqv. allocated(x)) STOP 29
     if (.not. allocated (x)) then
       allocate (x(1)[*])
       x(1)%i = 5
     else
       if (x(1)%i /= -3) STOP 30
       deallocate (x)
     end if
   end if
 end subroutine suba2c

 subroutine subac(x, prsnt, alloc)
   class(t), optional, allocatable :: x[:]
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) STOP 31
   if (present (x)) then
     if (alloc .neqv. allocated(x)) STOP 32
     if (.not. allocated (x)) then
       allocate (x[*])
       x%i = 5
     else
       if (x%i /= -3) STOP 33
       deallocate (x)
     end if
   end if
 end subroutine subac

 subroutine suba2(x, prsnt, alloc)
   class(t), optional, allocatable :: x(:)
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) STOP 34
   if (prsnt) then
     if (alloc .neqv. allocated(x)) STOP 35
     if (.not. allocated (x)) then
       allocate (x(1))
       x(1)%i = 5
     else
       if (x(1)%i /= -3) STOP 36
       deallocate (x)
     end if
   end if
 end subroutine suba2

 subroutine suba(x, prsnt, alloc)
   class(t), optional, allocatable :: x
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) STOP 37
   if (present (x)) then
     if (alloc .neqv. allocated(x)) STOP 38
     if (.not. allocated (x)) then
       allocate (x)
       x%i = 5
     else
       if (x%i /= -3) STOP 39
       deallocate (x)
     end if
   end if
 end subroutine suba

 subroutine subp2(x, prsnt, alloc)
   class(t), optional, pointer :: x(:)
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) STOP 40
   if (present (x)) then
     if (alloc .neqv. associated(x)) STOP 41
     if (.not. associated (x)) then
       allocate (x(1))
       x(1)%i = 5
     else
       if (x(1)%i /= -3) STOP 42
       deallocate (x)
     end if
   end if
 end subroutine subp2

 subroutine subp(x, prsnt, alloc)
   class(t), optional, pointer :: x
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) STOP 43
   if (present (x)) then
     if (alloc .neqv. associated(x)) STOP 44
     if (.not. associated (x)) then
       allocate (x)
       x%i = 5
     else
       if (x%i /= -3) STOP 45
       deallocate (x)
     end if
   end if
 end subroutine subp
end
