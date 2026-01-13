! { dg-do compile )
!
! Test the fix for PR112460, in which mismatched, constant typespec parameters were
! not detected.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module color_propagator
  implicit none
  integer, parameter :: pk = kind (1.0)
  type :: t (k, n_in, n_out)
     integer, kind :: k = pk
     integer, len :: n_in = 0, n_out = 0
     logical :: is_ghost = .false.
     integer, dimension(n_in) :: in
     integer, dimension(n_out) :: out
  end type t
end module color_propagator

program foo
  use color_propagator
  type(t(n_out=1)) :: aa
  type(t(n_in=1,n_out=2)) :: bb
  type(t), dimension(3) :: cc, dd, ee, gg
  type(t(pk,n_in=1,n_out=2)), dimension(3) :: ff, hh
  type(t(kind(1d0),n_in=1,n_out=2)), dimension(3) :: ii
  type(t(pk,n_in=1,n_out=1)), dimension(3) :: jj
  integer :: i

! Starting point was mismatched parameters in array constructors; eg.:
! Error: Mismatched type parameters ‘n_in’(1/0) in array constructor at (1)/(2)

  cc = [t(pk,1,1)(.true.,[5] ,[6]), aa, bb]     ! { dg-error "Mismatched type parameters" }
  dd = [aa, [t(pk,1,2)(.true.,[5] ,[6,6]), bb]] ! { dg-error "Mismatched type parameters" }
  ee = [bb, [t(pk,1,2)(.true.,[5],[6,6]), aa]]  ! { dg-error "Mismatched type parameters" }
  ff = [bb, [t(pk,1,2)(.true.,[5],[6,6]), bb]]  ! OK
  gg = [bb, [t(kind (1d0),1,2)(.true.,[5],[6,6]), bb]]  ! { dg-error "Mismatched type parameters" }

! Test ordinary assignment; eg.:
! Error: Mismatched type parameters ‘k’(8/4) in assignment at (1)/(2)

  aa = t(pk,1,2)(.true.,[5] ,[6,7])             ! { dg-error "Mismatched type parameters" }
  bb = t(pk,1,2)(.true.,[5] ,[6,7])             ! OK
  hh = ff                                       ! OK
  ii = ff                                       ! { dg-error "Mismatched type parameters" }
  jj = ff                                       ! { dg-error "Mismatched type parameters" }
  print *, ff
end program foo
