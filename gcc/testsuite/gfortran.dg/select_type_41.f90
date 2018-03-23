! { dg-do compile }
! { dg-options "-O2" }
!
! Tests the fix for PR80965 in which the use of the name 'loc'
! for the dummy argument of 'xyz' caused an ICE. If the module
! was used, the error "DUMMY attribute conflicts with INTRINSIC
! attribute in ‘loc’ at (1)" was emitted. Note that although 'loc'
! is a GNU extension and so can be over-ridden, this is not very
! good practice.
!
! Contributed by David Sagan  <david.sagan@gmail.com>
!
module mode3_mod
contains
  subroutine xyz (loc)
    implicit none
    class(*) :: loc
    real x(6)
    integer ix_use
    select type (loc)
      type is (integer)
        x = 0
        print *, "integer"
      type is (real)
        ix_use = 0
        print *, "real"
    end select
  end subroutine xyz
end module mode3_mod

