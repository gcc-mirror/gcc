! { dg-do compile }
! This tests the fix for PRs 26834, 25669 and 18803, in which
! shape information for the lbound and ubound intrinsics was not
! transferred to the scalarizer.  For this reason, an ICE would
! ensue, whenever these functions were used in temporaries.
!
! The tests are lifted from the PRs and some further checks are
! done to make sure that nothing is broken.
!
! This is PR26834
subroutine gfcbug34 ()
  implicit none
  type t
     integer, pointer :: i (:) => NULL ()
  end type t
  type(t), save :: gf
  allocate (gf%i(20))
  write(*,*) 'ubound:', ubound (gf% i)
  write(*,*) 'lbound:', lbound (gf% i)
end subroutine gfcbug34

! This is PR25669
subroutine foo (a)
  real a(*)
  call bar (a, LBOUND(a),2)
end subroutine foo
subroutine bar (b, i, j)
  real b(i:j)
  print *, i, j
  print *, b(i:j)
end subroutine bar

! This is PR18003
subroutine io_bug()
  integer :: a(10)
  print *, ubound(a)
end subroutine io_bug

! This checks that lbound and ubound are OK in  temporary
! expressions.
subroutine io_bug_plus()
  integer :: a(10, 10), b(2)
  print *, ubound(a)*(/1,2/)
  print *, (/1,2/)*ubound(a)
end subroutine io_bug_plus

  character(4) :: ch(2), ech(2) = (/'ABCD', 'EFGH'/)
  real(4) :: a(2)
  equivalence (ech,a)  ! { dg-warning "default CHARACTER EQUIVALENCE statement" }
  integer(1) :: i(8) = (/(j, j = 1,8)/)

! Check that the bugs have gone
  call io_bug ()
  call io_bug_plus ()
  call foo ((/1.0,2.0,3.0/))
  call gfcbug34 ()

! Check that we have not broken other intrinsics.
  print *, cos ((/1.0,2.0/))
  print *, transfer (a, ch)
  print *, i(1:4) * transfer (a, i, 4) * 2
end


