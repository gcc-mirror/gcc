! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! PR fortran/50981
! PR fortran/54618
! PR fortran/55978

  implicit none
  type t
   integer, allocatable :: i
  end type t
  type, extends (t):: t2
   integer, allocatable :: j
  end type t2

  call s1a1()
  call s1a()
  call s1ac1()
  call s1ac()
  call s2()
  call s2p(psnt=.false.)
  call s2caf()
  call s2elem()
  call s2elem_t()
  call s2elem_t2()
  call s2t()
  call s2tp(psnt=.false.)
  call s2t2()
  call s2t2p(psnt=.false.)

  call a1a1()
  call a1a()
  call a1ac1()
  call a1ac()
  call a2()
  call a2p(psnt=.false.)
  call a2caf()

  call a3a1()
  call a3a()
  call a3ac1()
  call a3ac()
  call a4()
  call a4p(psnt=.false.)
  call a4caf()

  call ar1a1()
  call ar1a()
  call ar1ac1()
  call ar1ac()
  call ar()
  call art()
  call arp(psnt=.false.)
  call artp(psnt=.false.)

contains

 subroutine s1a1(z, z2, z3, z4, z5)
   type(t), optional :: z, z4[*]
   type(t), pointer, optional :: z2
   type(t), allocatable, optional :: z3, z5[:]
   type(t), allocatable :: x
   type(t), pointer :: y
   y => null()
   call s2(x)
   call s2(y)
   call s2(z)
   call s2(z2)
   call s2(z3)
   call s2(z4)
   call s2(z5)
   call s2p(y,psnt=.true.)
   call s2p(z2,psnt=.false.)
   call s2elem(x)
   call s2elem(y)
   call s2elem(z)
   call s2elem(z2)
   call s2elem(z3)
   call s2elem(z4)
   call s2elem(z5)
   call s2elem_t(x)
   call s2elem_t(y)
   call s2elem_t(z)
!   call s2elem_t(z2) ! FIXME: Segfault
!   call s2elem_t(z3) ! FIXME: Segfault
!   call s2elem_t(z4) ! FIXME: Segfault
!   call s2elem_t(z5) ! FIXME: Segfault
   call s2caf(z4)
   call s2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
   call s2t(x)
   call s2t(y)
   call s2t(z)
!  call s2t(z2) ! FIXME: Segfault
!   call s2t(z3) ! FIXME: Segfault
!   call s2t(z4) ! FIXME: Segfault
!   call s2t(z5) ! FIXME: Segfault
   call s2tp(y,psnt=.true.)
   call s2tp(z2,psnt=.false.)
 end subroutine s1a1
 subroutine s1a(z, z2, z3, z4, z5)
   type(t2), optional :: z, z4[*]
   type(t2), optional, pointer :: z2
   type(t2), optional, allocatable :: z3, z5[:]
   type(t2), allocatable :: x
   type(t2), pointer :: y
   y => null()
   call s2(x)
   call s2(y)
   call s2(z)
   call s2(z2)
   call s2(z3)
   call s2(z4)
   call s2(z5)
   call s2p(y,psnt=.true.)
   call s2p(z2,psnt=.false.)
   call s2elem(x)
   call s2elem(y)
   call s2elem(z)
   call s2elem(z2)
   call s2elem(z3)
   call s2elem(z4)
   call s2elem(z5)
   call s2elem_t2(x)
   call s2elem_t2(y)
   call s2elem_t2(z)
!   call s2elem_t2(z2) ! FIXME: Segfault
!   call s2elem_t2(z3) ! FIXME: Segfault
!   call s2elem_t2(z4) ! FIXME: Segfault
!   call s2elem_t2(z5) ! FIXME: Segfault
   call s2caf(z4)
   call s2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
   call s2t2(x)
   call s2t2(y)
   call s2t2(z)
!   call s2t2(z2) ! FIXME: Segfault
!   call s2t2(z3) ! FIXME: Segfault
   call s2t2(z4)
!   call s2t2(z5) ! FIXME: Segfault
   call s2t2p(y,psnt=.true.)
   call s2t2p(z2,psnt=.false.)
 end subroutine s1a
 subroutine s1ac1(z, z2, z3, z4, z5)
   class(t), optional :: z, z4[*]
   class(t), optional, pointer :: z2
   class(t), optional, allocatable :: z3, z5[:]
   class(t), allocatable :: x
   class(t), pointer :: y
   y => null()
   call s2(x)
   call s2(y)
   call s2(z)
   call s2(z2)
   call s2(z3)
   call s2(z4)
   call s2(z5)
   call s2p(y,psnt=.true.)
   call s2p(z2,psnt=.false.)
   call s2elem(x)
   call s2elem(y)
   call s2elem(z)
   call s2elem(z2)
   call s2elem(z3)
   call s2elem(z4)
   call s2elem(z5)
   call s2elem_t(x)
   call s2elem_t(y)
!   call s2elem_t(z) ! FIXME: Segfault
!   call s2elem_t(z2) ! FIXME: Segfault
!   call s2elem_t(z3) ! FIXME: Segfault
!   call s2elem_t(z4) ! FIXME: Segfault
!   call s2elem_t(z5) ! FIXME: Segfault
   call s2caf(z4)
   call s2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
   call s2t(x)
   call s2t(y)
!   call s2t(z) ! FIXME: Segfault
!   call s2t(z2) ! FIXME: Segfault
!   call s2t(z3) ! FIXME: Segfault
!   call s2t(z4) ! FIXME: Segfault
!   call s2t(z5) ! FIXME: Segfault
   call s2tp(y,psnt=.true.)
   call s2tp(z2,psnt=.false.)
 end subroutine s1ac1
 subroutine s1ac(z, z2, z3, z4, z5)
   class(t2), optional :: z, z4[*]
   class(t2), optional, pointer :: z2
   class(t2), optional, allocatable :: z3, z5[:]
   class(t2), allocatable :: x
   class(t2), pointer :: y
   y => null()
   call s2(x)
   call s2(y)
   call s2(z)
   call s2(z2)
   call s2(z3)
   call s2(z4)
   call s2(z5)
   call s2p(y,psnt=.true.)
   call s2p(z2,psnt=.false.)
   call s2elem(x)
   call s2elem(y)
   call s2elem(z)
   call s2elem(z2)
   call s2elem(z3)
   call s2elem(z4)
   call s2elem(z5)
   call s2elem_t2(x)
!   call s2elem_t2(y) ! FIXME: Segfault
!   call s2elem_t2(z) ! FIXME: Segfault
!   call s2elem_t2(z2) ! FIXME: Segfault
!   call s2elem_t2(z3) ! FIXME: Segfault
!   call s2elem_t2(z4) ! FIXME: Segfault
!   call s2elem_t2(z5) ! FIXME: Segfault
   call s2caf(z4)
   call s2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
   call s2t2(x)
   call s2t2(y)
!   call s2t2(z) ! FIXME: Segfault
!   call s2t2(z2) ! FIXME: Segfault
!   call s2t2(z3) ! FIXME: Segfault
!   call s2t2(z4) ! FIXME: Segfault
!   call s2t2(z5) ! FIXME: Segfault
   call s2t2p(y,psnt=.true.)
   call s2t2p(z2,psnt=.false.)
 end subroutine s1ac

 subroutine s2(x)
   class(t), intent(in), optional :: x
   if (present (x)) STOP 1
   !print *, present(x)
 end subroutine s2
 subroutine s2p(x,psnt)
   class(t), intent(in), pointer, optional :: x
   logical psnt
   if (present (x).neqv. psnt) STOP 2
   !print *, present(x)
 end subroutine s2p
 subroutine s2caf(x)
   class(t), intent(in), optional :: x[*]
   if (present (x)) STOP 3
   !print *, present(x)
 end subroutine s2caf
 subroutine s2t(x)
   type(t), intent(in), optional :: x
   if (present (x)) STOP 4
   !print *, present(x)
 end subroutine s2t
 subroutine s2t2(x)
   type(t2), intent(in), optional :: x
   if (present (x)) STOP 5
   !print *, present(x)
 end subroutine s2t2
 subroutine s2tp(x, psnt)
   type(t), pointer, intent(in), optional :: x
   logical psnt
   if (present (x).neqv. psnt) STOP 6
   !print *, present(x)
 end subroutine s2tp
 subroutine s2t2p(x, psnt)
   type(t2), pointer, intent(in), optional :: x
   logical psnt
   if (present (x).neqv. psnt) STOP 7
   !print *, present(x)
 end subroutine s2t2p
 impure elemental subroutine s2elem(x)
   class(t), intent(in), optional :: x
   if (present (x)) STOP 8
   !print *, present(x)
 end subroutine s2elem
 impure elemental subroutine s2elem_t(x)
   type(t), intent(in), optional :: x
   if (present (x)) STOP 9
   !print *, present(x)
 end subroutine s2elem_t
 impure elemental subroutine s2elem_t2(x)
   type(t2), intent(in), optional :: x
   if (present (x)) STOP 10
   !print *, present(x)
 end subroutine s2elem_t2


 subroutine a1a1(z, z2, z3, z4, z5)
   type(t), optional :: z(:), z4(:)[*]
   type(t), optional, pointer :: z2(:)
   type(t), optional, allocatable :: z3(:), z5(:)[:]
   type(t), allocatable :: x(:)
   type(t), pointer :: y(:)
   y => null()
   call a2(x)
   call a2(y)
   call a2(z)
   call a2(z2)
   call a2(z3)
   call a2(z4)
   call a2(z5)
   call a2p(y,psnt=.true.)
   call a2p(z2,psnt=.false.)
   call a2caf(z4)
   call a2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
!   call s2elem(x) ! FIXME: Segfault
!   call s2elem(y) ! FIXME: Segfault
!   call s2elem(z) ! FIXME: Segfault
!   call s2elem(z2) ! FIXME: Segfault
!   call s2elem(z3) ! FIXME: Segfault
!   call s2elem(z4) ! FIXME: Segfault
!   call s2elem(z5) ! FIXME: Segfault
!   call s2elem_t(x) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t(y) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t(z) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t(z2) ! FIXME: Segfault
!   call s2elem_t(z3) ! FIXME: Segfault
!   call s2elem_t(z4) ! FIXME: Segfault
!   call s2elem_t(z5) ! FIXME: Segfault
 end subroutine a1a1
 subroutine a1a(z, z2, z3, z4, z5)
   type(t2), optional :: z(:), z4(:)[*]
   type(t2), optional, pointer :: z2(:)
   type(t2), optional, allocatable :: z3(:), z5(:)[:]
   type(t2), allocatable :: x(:)
   type(t2), pointer :: y(:)
   y => null()
   call a2(x)
   call a2(y)
   call a2(z)
   call a2(z2)
   call a2(z3)
   call a2(z4)
   call a2(z5)
   call a2p(y,psnt=.true.)
   call a2p(z2,psnt=.false.)
   call a2caf(z4)
   call a2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
!   call s2elem(x) ! FIXME: Segfault
!   call s2elem(y) ! FIXME: Segfault
!   call s2elem(z) ! FIXME: Segfault
!   call s2elem(z2) ! FIXME: Segfault
!   call s2elem(z3) ! FIXME: Segfault
!   call s2elem(z4) ! FIXME: Segfault
!   call s2elem(z5) ! FIXME: Segfault
!   call s2elem_t2(x) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t2(y) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t2(z) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t2(z2) ! FIXME: Segfault
!   call s2elem_t2(z3) ! FIXME: Segfault
!   call s2elem_t2(z4) ! FIXME: Segfault
!   call s2elem_t2(z5) ! FIXME: Segfault
 end subroutine a1a
 subroutine a1ac1(z, z2, z3, z4, z5)
   class(t), optional :: z(:), z4(:)[*]
   class(t), optional, pointer :: z2(:)
   class(t), optional, allocatable :: z3(:), z5(:)[:]
   class(t), allocatable :: x(:)
   class(t), pointer :: y(:)
   y => null()
   call a2(x)
   call a2(y)
   call a2(z)
   call a2(z2)
   call a2(z3)
   call a2(z4)
   call a2(z5)
   call a2p(y,psnt=.true.)
   call a2p(z2,psnt=.false.)
   call a2caf(z4)
   call a2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
!   call s2elem(x) ! FIXME: Segfault
!   call s2elem(y) ! FIXME: Segfault
!   call s2elem(z) ! FIXME: Segfault
!   call s2elem(z2) ! FIXME: Segfault
!   call s2elem(z3) ! FIXME: Segfault
!   call s2elem(z4) ! FIXME: Segfault
!   call s2elem(z5) ! FIXME: Segfault
!   call s2elem_t(x) ! FIXME: Segfault
!   call s2elem_t(y) ! FIXME: Segfault
!   call s2elem_t(z) ! FIXME: Segfault
!   call s2elem_t(z2) ! FIXME: Segfault
!   call s2elem_t(z3) ! FIXME: Segfault
!   call s2elem_t(z4) ! FIXME: Segfault
!   call s2elem_t(z5) ! FIXME: Segfault
 end subroutine a1ac1
 subroutine a1ac(z, z2, z3, z4, z5)
   class(t2), optional :: z(:), z4(:)[*]
   class(t2), optional, pointer :: z2(:)
   class(t2), optional, allocatable :: z3(:), z5(:)[:]
   class(t2), allocatable :: x(:)
   class(t2), pointer :: y(:)
   y => null()
   call a2(x)
   call a2(y)
   call a2(z)
   call a2(z2)
   call a2(z3)
   call a2(z4)
   call a2(z5)
   call a2p(y,psnt=.true.)
   call a2p(z2,psnt=.false.)
   call a2caf(z4)
   call a2caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
!   call s2elem(x) ! FIXME: Segfault
!   call s2elem(y) ! FIXME: Segfault
!   call s2elem(z) ! FIXME: Segfault
!   call s2elem(z2) ! FIXME: Segfault
!   call s2elem(z3) ! FIXME: Segfault
!   call s2elem(z4) ! FIXME: Segfault
!   call s2elem(z5) ! FIXME: Segfault
!   call s2elem_t2(x) ! FIXME: Segfault
!   call s2elem_t2(y) ! FIXME: Segfault
!   call s2elem_t2(z) ! FIXME: Segfault
!   call s2elem_t2(z2) ! FIXME: Segfault
!   call s2elem_t2(z3) ! FIXME: Segfault
!   call s2elem_t2(z4) ! FIXME: Segfault
!   call s2elem_t2(z5) ! FIXME: Segfault
 end subroutine a1ac

 subroutine a2(x)
   class(t), intent(in), optional :: x(:)
   if (present (x)) STOP 11
   ! print *, present(x)
 end subroutine a2
 subroutine a2p(x, psnt)
   class(t), pointer, intent(in), optional :: x(:)
   logical psnt
   if (present (x).neqv. psnt) STOP 12
   ! print *, present(x)
 end subroutine a2p
 subroutine a2caf(x)
   class(t), intent(in), optional :: x(:)[*]
   if (present (x)) STOP 13
   ! print *, present(x)
 end subroutine a2caf


 subroutine a3a1(z, z2, z3, z4, z5)
   type(t), optional :: z(4), z4(4)[*]
   type(t), optional, pointer :: z2(:)
   type(t), optional, allocatable :: z3(:), z5(:)[:]
   type(t), allocatable :: x(:)
   type(t), pointer :: y(:)
   y => null()
   call a4(x)
   call a4(y)
   call a4(z)
   call a4(z2)
   call a4(z3)
   call a4(z4)
   call a4(z5)
   call a4p(y,psnt=.true.)
   call a4p(z2,psnt=.false.)
   call a4t(x)
   call a4t(y)
   call a4t(z)
!   call a4t(z2) ! FIXME: Segfault
!   call a4t(z3) ! FIXME: Segfault
!   call a4t(z4) ! FIXME: Segfault
!   call a4t(z5) ! FIXME: Segfault
   call a4tp(y,psnt=.true.)
   call a4tp(z2,psnt=.false.)
   call a4caf(z4)
   call a4caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
!   call s2elem(x) ! FIXME: Segfault
!   call s2elem(y) ! FIXME: Segfault
!   call s2elem(z) ! FIXME: Segfault
!   call s2elem(z2) ! FIXME: Segfault
!   call s2elem(z3) ! FIXME: Segfault
!   call s2elem(z4) ! FIXME: Segfault
!   call s2elem(z5) ! FIXME: Segfault
!   call s2elem_t(x) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t(y) ! FIXME: Conditional jump or move depends on uninitialised value
   call s2elem_t(z)
!   call s2elem_t(z2) ! FIXME: Segfault
!   call s2elem_t(z3) ! FIXME: Segfault
!   call s2elem_t(z4) ! FIXME: Segfault
!   call s2elem_t(z5) ! FIXME: Segfault
 end subroutine a3a1
 subroutine a3a(z, z2, z3)
   type(t2), optional :: z(4)
   type(t2), optional, pointer :: z2(:)
   type(t2), optional, allocatable :: z3(:)
   type(t2), allocatable :: x(:)
   type(t2), pointer :: y(:)
   y => null()
   call a4(x)
   call a4(y)
   call a4(z)
   call a4(z2)
   call a4(z3)
   call a4p(y,psnt=.true.)
   call a4p(z2,psnt=.false.)
   call a4t2(x)
   call a4t2(y)
   call a4t2(z)
!   call a4t2(z2) ! FIXME: Segfault
!   call a4t2(z3) ! FIXME: Segfault
   call a4t2p(y,psnt=.true.)
   call a4t2p(z2,psnt=.false.)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
!   call s2elem(x) ! FIXME: Segfault
!   call s2elem(y) ! FIXME: Segfault
!   call s2elem(z) ! FIXME: Segfault
!   call s2elem(z2) ! FIXME: Segfault
!   call s2elem(z3) ! FIXME: Segfault
!   call s2elem(z4) ! FIXME: Segfault
!   call s2elem(z5) ! FIXME: Segfault
!   call s2elem_t2(x) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t2(y) ! FIXME: Conditional jump or move depends on uninitialised value
   call s2elem_t2(z)
!   call s2elem_t2(z2) ! FIXME: Segfault
!   call s2elem_t2(z3) ! FIXME: Segfault
!   call s2elem_t2(z4) ! FIXME: Segfault
!   call s2elem_t2(z5) ! FIXME: Segfault
 end subroutine a3a
 subroutine a3ac1(z, z2, z3, z4, z5)
   class(t), optional :: z(4), z4(4)[*]
   class(t), optional, pointer :: z2(:)
   class(t), optional, allocatable :: z3(:), z5(:)[:]
   class(t), allocatable :: x(:)
   class(t), pointer :: y(:)
   y => null()
   call a4(x)
   call a4(y)
   call a4(z)
   call a4(z2)
   call a4(z3)
   call a4(z4)
   call a4(z5)
   call a4p(y,psnt=.true.)
   call a4p(z2,psnt=.false.)
!   call a4t(x) ! FIXME: Segfault
!   call a4t(y) ! FIXME: Segfault
!   call a4t(z) ! FIXME: Segfault
!   call a4t(z2) ! FIXME: Segfault
!   call a4t(z3) ! FIXME: Segfault
!   call a4t(z4) ! FIXME: Segfault
!   call a4t(z5) ! FIXME: Segfault
!   call a4tp(y,psnt=.true.) ! FIXME: Segfault
!   call a4tp(z2,psnt=.false.) ! FIXME: Segfault
   call a4caf(z4)
   call a4caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.)
   call arp(z2,psnt=.false.)
!   call s2elem(x) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem(y) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem(z) ! FIXME: Segfault
!   call s2elem(z2) ! FIXME: Segfault
!   call s2elem(z3) ! FIXME: Segfault
!   call s2elem(z4) ! FIXME: Segfault
!   call s2elem(z5) ! FIXME: Segfault
!   call s2elem_t(x) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t(y) ! FIXME: Conditional jump or move depends on uninitialised value
!   call s2elem_t(z) ! FIXME: Segfault
!   call s2elem_t(z2) ! FIXME: Segfault
!   call s2elem_t(z3) ! FIXME: Segfault
!   call s2elem_t(z4) ! FIXME: Segfault
!   call s2elem_t(z5) ! FIXME: Segfault
 end subroutine a3ac1
 subroutine a3ac(z, z2, z3, z4, z5)
   class(t2), optional :: z(4), z4(4)[*]
   class(t2), optional, pointer :: z2(:)
   class(t2), optional, allocatable :: z3(:), z5(:)[:]
   class(t2), allocatable :: x(:)
   class(t2), pointer :: y(:)
   y => null()
   call a4(x)
   call a4(y)
   call a4(z)
   call a4(z2)
   call a4(z3)
   call a4(z4)
   call a4(z5)
   call a4p(y,psnt=.true.)
   call a4p(z2,psnt=.false.)
!   call a4t2(x) ! FIXME: Segfault
!   call a4t2(y) ! FIXME: Segfault
!   call a4t2(z) ! FIXME: Segfault
!   call a4t2(z2) ! FIXME: Segfault
!   call a4t2(z3) ! FIXME: Segfault
!   call a4t2(z4) ! FIXME: Segfault
!   call a4t2(z5) ! FIXME: Segfault
!   call a4t2p(y,psnt=.true.) ! FIXME: Segfault
!   call a4t2p(z2,psnt=.false.) ! FIXME: Segfault
   call a4caf(z4)
   call a4caf(z5)
   call ar(x)
   call ar(y)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call ar(z4)
   call ar(z5)
   call arp(y,psnt=.true.) 
   call arp(z2,psnt=.false.)
 end subroutine a3ac

 subroutine a4(x)
   class(t), intent(in), optional :: x(4)
   if (present (x)) STOP 14
   !print *, present(x)
 end subroutine a4
 subroutine a4p(x, psnt)
   class(t), pointer, intent(in), optional :: x(:)
   logical psnt
   if (present (x).neqv. psnt) STOP 15
   !print *, present(x)
 end subroutine a4p
 subroutine a4caf(x)
   class(t), intent(in), optional :: x(4)[*]
   if (present (x)) STOP 16
   !print *, present(x)
 end subroutine a4caf
 subroutine a4t(x)
   type(t), intent(in), optional :: x(4)
   if (present (x)) STOP 17
   !print *, present(x)
 end subroutine a4t
 subroutine a4t2(x)
   type(t2), intent(in), optional :: x(4)
   if (present (x)) STOP 18
   !print *, present(x)
 end subroutine a4t2
 subroutine a4tp(x, psnt)
   type(t), pointer, intent(in), optional :: x(:)
   logical psnt
   if (present (x).neqv. psnt) STOP 19
   !print *, present(x)
 end subroutine a4tp
 subroutine a4t2p(x, psnt)
   type(t2), pointer, intent(in), optional :: x(:)
   logical psnt
   if (present (x).neqv. psnt) STOP 20
   !print *, present(x)
 end subroutine a4t2p


 subroutine ar(x)
   class(t), intent(in), optional :: x(..)
   if (present (x)) STOP 21
   !print *, present(x)
 end subroutine ar

 subroutine art(x)
   type(t), intent(in), optional :: x(..)
   if (present (x)) STOP 22
   !print *, present(x)
 end subroutine art

 subroutine arp(x, psnt)
   class(t), pointer, intent(in), optional :: x(..)
   logical psnt
   if (present (x).neqv. psnt) STOP 23
   !print *, present(x)
 end subroutine arp

 subroutine artp(x, psnt)
   type(t), intent(in), pointer, optional :: x(..)
   logical psnt
   if (present (x).neqv. psnt) STOP 24
   !print *, present(x)
 end subroutine artp



 subroutine ar1a1(z, z2, z3)
   type(t), optional :: z(..)
   type(t), pointer, optional :: z2(..)
   type(t), allocatable, optional :: z3(..)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call art(z)
   call art(z2)
   call art(z3)
   call arp(z2, .false.)
   call artp(z2, .false.)
 end subroutine ar1a1
 subroutine ar1a(z, z2, z3)
   type(t2), optional :: z(..)
   type(t2), optional, pointer :: z2(..)
   type(t2), optional, allocatable :: z3(..)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call arp(z2, .false.)
 end subroutine ar1a
 subroutine ar1ac1(z, z2, z3)
   class(t), optional :: z(..)
   class(t), optional, pointer :: z2(..)
   class(t), optional, allocatable :: z3(..)
   call ar(z)
   call ar(z2)
   call ar(z3)
!   call art(z) ! FIXME: ICE - This requires packing support for assumed-rank
!   call art(z2)! FIXME: ICE - This requires packing support for assumed-rank
!   call art(z3)! FIXME: ICE - This requires packing support for assumed-rank
   call arp(z2, .false.)
!   call artp(z2, .false.) ! FIXME: ICE
 end subroutine ar1ac1
 subroutine ar1ac(z, z2, z3)
   class(t2), optional :: z(..)
   class(t2), optional, pointer :: z2(..)
   class(t2), optional, allocatable :: z3(..)
   call ar(z)
   call ar(z2)
   call ar(z3)
   call arp(z2, .false.)
 end subroutine ar1ac
end
