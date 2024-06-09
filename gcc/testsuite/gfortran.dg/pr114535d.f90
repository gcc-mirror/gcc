! { dg-do compile }
! { dg-compile-aux-modules "pr114535iv.f90" }
! Contributed by Andrew Benson  <abensonca@gcc.gnu.org>
!
module d
  implicit none
contains
  function en() result(dd)
    use :: iv
    implicit none
    type(vs) :: dd
    dd%i = 1
  end function en
end module d

! Delete line 1 and all brands complain that 'vs' is an undefined type.
! Delete lines 1 and line 2 recreates the original problem.
module ni
  implicit none
contains
  subroutine iss1()
!    use :: iv                                        ! line 1
    use :: d
    implicit none
!    type(vs) :: ans; ans = en();                     ! line 2
  end subroutine iss1
  subroutine iss2()
    use :: d
    implicit none
  end subroutine iss2
end module ni ! Used to give an ICE: in gfc_trans_call, at fortran/trans-stmt.cc:400

  use ni
  use iv
  type(vs) :: x
  call iss1()
  call iss1()
  if ((ctr .eq. 0) .or. (ctr .ne. 6)) stop 1  ! Depends whether lines 1 & 2 are present
  call iss2()
  x = vs(42)
  if ((ctr .eq. 1) .or. (ctr .ne. 7)) stop 2  ! Make sure destructor available here
end
