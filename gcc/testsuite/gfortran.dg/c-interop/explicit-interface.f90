! { dg-do compile }
!
! TS 29113
! 6.2 Explicit interface
! 
! Additionally to the rules of subclause 12.4.2.2 of ISO/IEC 1539-1:2010,
! a procedure shall have an explicit interface if it has a dummy argument
! that is assumed-rank.
!
! NOTE 6.1
! An explicit interface is also required for a procedure if it has a
! dummy argument that is assumed-type because an assumed-type dummy 
! argument is polymorphic.
!
! This file contains code that is expected to produce errors.

module m1

  interface

    subroutine s1 (a)
      integer :: a(..)
    end subroutine

    subroutine s2 (b)
      type(*) :: b
    end subroutine

  end interface

end module

module m2

  contains

  ! This subroutine has an explicit interface, and so do the things
  ! it calls.
  subroutine good (a, b)
    use m1
    integer :: a(..)
    type (*) :: b

    call s1 (a)
    call s2 (b)
  end subroutine

  ! This subroutine has an explicit interface, but the things it calls don't.
  subroutine bad (a, b)
    use m1
    integer :: a(..)
    type (*) :: b
    external :: s3, s4

    call s3 (a)  ! { dg-error "Assumed-rank argument" }
    call s4 (b)  ! { dg-error "Assumed-type argument" }
  end subroutine

end module

