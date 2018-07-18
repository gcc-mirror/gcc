! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/48820
!
! Ensure that the value of scalars to assumed-rank arrays is
! copied back, if and only its pointer address could have changed.
!
program test
 implicit none
 type t
   integer :: aa
 end type t

 integer, allocatable :: iia
 integer, pointer     :: iip

 type(t), allocatable :: jja
 type(t), pointer     :: jjp

 logical :: is_present

 is_present = .true.

 allocate (iip, jjp)

 iia = 7
 iip = 7
 jja = t(88)
 jjp = t(88)

 call faa(iia, jja) ! Copy back
 if (iia /= 7 .and. jja%aa /= 88) STOP 1
 call fai(iia, jja) ! No copy back
 if (iia /= 7 .and. jja%aa /= 88) STOP 2

 call fpa(iip, jjp) ! Copy back
 if (iip /= 7 .and. jjp%aa /= 88) STOP 3
 call fpi(iip, jjp) ! No copy back
 if (iip /= 7 .and. jjp%aa /= 88) STOP 4

 call fnn(iia, jja) ! No copy back
 if (iia /= 7 .and. jja%aa /= 88) STOP 5
 call fno(iia, jja) ! No copy back
 if (iia /= 7 .and. jja%aa /= 88) STOP 6
 call fnn(iip, jjp) ! No copy back
 if (iip /= 7 .and. jjp%aa /= 88) STOP 7
 call fno(iip, jjp) ! No copy back
 if (iip /= 7 .and. jjp%aa /= 88) STOP 8

 is_present = .false.

 call fpa(null(), null()) ! No copy back
 call fpi(null(), null()) ! No copy back
 call fno(null(), null()) ! No copy back

 call fno() ! No copy back

contains

  subroutine faa (xx1, yy1)
    integer, allocatable :: xx1(..)
    type(t), allocatable :: yy1(..)
    if (.not. allocated (xx1)) STOP 9
    if (.not. allocated (yy1)) STOP 10
  end subroutine faa
  subroutine fai (xx1, yy1)
    integer, allocatable, intent(in) :: xx1(..)
    type(t), allocatable, intent(in) :: yy1(..)
    if (.not. allocated (xx1)) STOP 11
    if (.not. allocated (yy1)) STOP 12
  end subroutine fai
  subroutine fpa (xx1, yy1)
    integer, pointer :: xx1(..)
    type(t), pointer :: yy1(..)
    if (is_present .neqv. associated (xx1)) STOP 13
    if (is_present .neqv. associated (yy1)) STOP 14
  end subroutine fpa

  subroutine fpi (xx1, yy1)
    integer, pointer, intent(in) :: xx1(..)
    type(t), pointer, intent(in) :: yy1(..)
    if (is_present .neqv. associated (xx1)) STOP 15
    if (is_present .neqv. associated (yy1)) STOP 16
  end subroutine fpi

  subroutine fnn(xx2,yy2)
    integer  :: xx2(..)
    type(t)  :: yy2(..)
  end subroutine fnn

  subroutine fno(xx2,yy2)
    integer, optional  :: xx2(..)
    type(t), optional  :: yy2(..)
    if (is_present .neqv. present (xx2)) STOP 17
    if (is_present .neqv. present (yy2)) STOP 18
  end subroutine fno
end program test

! We should have exactly one copy back per variable
!
! { dg-final { scan-tree-dump-times "iip = .integer.kind=4. .. desc.\[0-9\]+.data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "iia = .integer.kind=4. .. desc.\[0-9\]+.data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "jjp = .struct t .. desc.\[0-9\]+.data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "jja = .struct t .. desc.\[0-9\]+.data;" 1 "original" } }
