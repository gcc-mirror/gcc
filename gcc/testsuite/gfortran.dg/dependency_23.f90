! { dg-do run }
! Test the fix for PR38863, in which an unnecessary temporary
! generated results that are not consistent with other compilers.
!
! Contributed by Dick Hendrickson  <dick.hendrickson@gmail.com>
!
module rg0045_stuff
  type unseq
    integer :: i
    logical :: l
  end type unseq
  interface assignment(=)
    module procedure l_to_t, i_to_t
  end interface
contains
  elemental subroutine l_to_t (arg1, arg2)
    type(unseq), intent(inout) :: arg1
    logical, intent(in) :: arg2
    arg1%l = arg2
  end subroutine l_to_t
  elemental subroutine i_to_t (arg1, arg2)
    type(unseq), intent(inout) :: arg1
    integer, intent(in) :: arg2
    arg1%i = arg2
  end subroutine i_to_t
  subroutine rg0045(nf1, nf2, nf3)
    type(unseq) :: tla2l(nf3, nf2)
    type(unseq) :: tda2l(3,2)
    logical :: lda(nf3,nf2)
    tda2l%l = reshape ([.true.,.false.,.true.,.false.,.true.,.false.],[3,2])
    tda2l%i = reshape ([1, -1, 3, -1, 5, -1],[3,2])
    lda = tda2l%l
    tla2l%l = lda
    tla2l%i = reshape ([1, 2, 3, 4, 5, 6], [3,2])
!
! The problem occurred here: gfortran was producing a temporary for these
! assignments because the dependency checking was too restrictive.  Since
! a temporary was used, the integer component was reset in the first assignment
! rather than being carried over.
!
    where(lda)
      tla2l = tla2l(1:3, 1:2)%l
      tla2l = tla2l(1:3, 1:2)%i
    elsewhere
      tla2l = -1
    endwhere
    if (any (tla2l%i .ne. tda2l%i)) call abort
    if (any (tla2l%l .neqv. tda2l%l)) call abort
  end subroutine
end module rg0045_stuff

  use rg0045_stuff
  call rg0045(1, 2, 3)
end
! { dg-final { cleanup-modules "rg0045_stuff" } }


