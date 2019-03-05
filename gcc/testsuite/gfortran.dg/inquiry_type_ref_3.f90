! { dg-do compile }
!
! Test the implementation of inquiry part references (PR40196):
! Check errors on invalid code.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
program main
   type :: t
     complex :: z
     character(6) :: a
   end type
   character(4) :: a
   character(:), allocatable :: b
   real :: z
   integer :: i
   type(t) :: s
   b = "abcdefg"
   a%kind = 2        ! { dg-error "Assignment to a constant expression" }
   b%len = 2         ! { dg-error "Assignment to a LEN or KIND part_ref" }
   i = a%kind        ! OK
   i = b%len         ! OK
   print *, z%re     ! { dg-error "must be applied to a COMPLEX expression" }
   print *, z%im     ! { dg-error "must be applied to a COMPLEX expression" }
   i%re = 2.0        ! { dg-error "must be applied to a COMPLEX expression" }
   print *, i%len    ! { dg-error "must be applied to a CHARACTER expression" }
   print *, s%kind   ! { dg-error "is not a member" }
   print *, s%z%kind ! OK
   print *, s%a%len  ! OK
end
