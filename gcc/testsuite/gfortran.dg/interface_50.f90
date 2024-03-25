! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Tests the fix for PR98498, which was subject to an interpretation request
! as to whether or not the interface operator overrode the intrinsic use.
! (See PR for correspondence)
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
MODULE mytypes
  IMPLICIT none

  TYPE pvar
     character(len=20) :: name
     integer           :: level
  end TYPE pvar

  interface operator (==)
     module procedure star_eq
  end interface

  interface operator (.not.)
     module procedure star_not
  end interface

contains
  function star_eq(a, b)
    implicit none
    class(*), intent(in) :: a, b
    logical :: star_eq
    select type (a)
      type is (pvar)
      select type (b)
        type is (pvar)
          if((a%level .eq. b%level) .and. (a%name .eq. b%name)) then
            star_eq = .true.
          else
            star_eq = .false.
          end if
        type is (integer)
          star_eq = (a%level == b)
      end select
      class default
        star_eq = .false.
    end select
  end function star_eq

  function star_not (a)
    implicit none
    class(*), intent(in) :: a
    type(pvar) :: star_not
    select type (a)
      type is (pvar)
        star_not = a
        star_not%level = -star_not%level
      type is (real)
        star_not = pvar ("real", -int(a))
      class default
        star_not = pvar ("noname", 0)
    end select
  end function

end MODULE mytypes

program test_eq
   use mytypes
   implicit none

   type(pvar) x, y
   integer :: i = 4
   real :: r = 2.0
   character(len = 4, kind =4) :: c = "abcd"
! Check that intrinsic use of .not. and == is not overridden.
   if (.not.(i == 2*int (r))) stop 1
   if (r == 1.0) stop 2

! Test defined operator ==
   x = pvar('test 1', 100)
   y = pvar('test 1', 100)
   if (.not.(x == y)) stop 3
   y = pvar('test 2', 100)
   if (x == y) stop 4
   if (x == r) stop 5            ! class default gives .false.
   if (100 == x) stop 6          !       ditto
   if (.not.(x == 100)) stop 7   ! integer selector gives a%level == b
   if (i == "c") stop 8          ! type mismatch => calls star_eq
   if (c == "abcd") stop 9       ! kind mismatch => calls star_eq

! Test defined operator .not.
   y = .not.x
   if (y%level .ne. -x%level) stop 11
   y = .not.i
   if (y%level .ne. 0 .and. trim(y%name) .ne. "noname") stop 12
   y = .not.r
   if (y%level .ne. -2 .and. trim(y%name) .ne. "real") stop 13
end program test_eq
! { dg-final { scan-tree-dump-times "star_eq" 14 "original" } }
! { dg-final { scan-tree-dump-times "star_not" 11 "original" } }
