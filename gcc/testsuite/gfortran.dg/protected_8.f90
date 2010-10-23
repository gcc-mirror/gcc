! { dg-do compile }
!
! PR fortran/46122
!
! PROTECT check
!
! Contributed by Jared Ahern
!

MODULE amod
   IMPLICIT NONE
   TYPE foo
      INTEGER :: i = 4
      INTEGER, POINTER :: j => NULL()
   END TYPE foo
   TYPE(foo), SAVE, PROTECTED :: a
   TYPE(foo), SAVE, PROTECTED, POINTER :: b
   INTEGER, SAVE, PROTECTED :: i = 5
   INTEGER, SAVE, PROTECTED, POINTER :: j => NULL()
contains
  subroutine alloc()
    allocate(b,j)
  end subroutine alloc
END MODULE amod

PROGRAM test
   USE amod
   IMPLICIT NONE
   INTEGER, TARGET :: k
   TYPE(foo), TARGET :: c
   k = 2   ! local
   c%i = 9 ! local

   call alloc()

   i = k    ! { dg-error "is PROTECTED" }
   j => k   ! { dg-error "is PROTECTED" }
   j = 3    ! OK 1
   a = c    ! { dg-error "is PROTECTED" }
   a%i = k  ! { dg-error "is PROTECTED" }
   a%j => k ! { dg-error "is PROTECTED" }
   a%j = 5  ! OK 2
   b => c   ! { dg-error "is PROTECTED" }
   b%i = k  ! OK 3
   b%j => k ! OK 4
   b%j = 5  ! OK 5

END PROGRAM test

! { dg-final { cleanup-modules "amod" } }
