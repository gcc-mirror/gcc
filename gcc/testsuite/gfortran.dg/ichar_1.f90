! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR20879
! Check that we reject expressions longer than one character for the
! ICHAR and IACHAR intrinsics.

! Assumed length variables are special because the frontend doesn't have
! an expression for their length
subroutine test (c)
  character(len=*) :: c
  integer i
  i = ichar(c)
  i = ichar(c(2:))
  i = ichar(c(:1))
end subroutine

program ichar_1
   type derivedtype
      character(len=4) :: addr
   end type derivedtype

   type derivedtype1
      character(len=1) :: addr
   end type derivedtype1

   integer i
   integer, parameter :: j = 2
   character(len=8) :: c = 'abcd'
   character(len=1) :: g1(2)
   character(len=1) :: g2(2,2)
   character*1, parameter :: s1 = 'e'
   character*2, parameter :: s2 = 'ef'
   type(derivedtype) :: dt
   type(derivedtype1) :: dt1

   if (ichar(c(3:3)) /= 97) STOP 1
   if (ichar(c(:1)) /= 97) STOP 2
   if (ichar(c(j:j)) /= 98) STOP 3
   if (ichar(s1) /= 101) STOP 4
   if (ichar('f') /= 102) STOP 5
   g1(1) = 'a'
   if (ichar(g1(1)) /= 97) STOP 6
   if (ichar(g1(1)(:)) /= 97) STOP 7
   g2(1,1) = 'a'
   if (ichar(g2(1,1)) /= 97) STOP 8

   i = ichar(c)      ! { dg-error "must be of length one" }
   i = ichar(c(:))   ! { dg-error "must be of length one" }
   i = ichar(s2)     ! { dg-error "must be of length one" }
   i = ichar(c(1:2)) ! { dg-error "must be of length one" }
   i = ichar(c(1:))  ! { dg-error "must be of length one" }
   i = ichar('abc')  ! { dg-error "must be of length one" }

   ! ichar and iachar use the same checking routines. DO a couple of tests to
   ! make sure it's not totally broken.

   if (ichar(c(3:3)) /= 97) STOP 9
   i = ichar(c)      ! { dg-error "must be of length one" }
   
   i = ichar(dt%addr(1:1))
   i = ichar(dt%addr) ! { dg-error "must be of length one" }
   i = ichar(dt%addr(1:2)) ! { dg-error "must be of length one" }
   i = ichar(dt%addr(1:)) ! { dg-error "must be of length one" }
   
   i = ichar(dt1%addr(1:1))
   i = ichar(dt1%addr)


   call test(g1(1))
end program ichar_1
