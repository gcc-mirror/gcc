! { dg-do run }
! Fix STORAGE_SIZE intrinsic for polymorphic arguments PR84006 and PR100027.
! Contributed by Steve Kargl  <kargls@comcast.net>
!            and José Rui Faustino de Sousa  <jrfsousa@gcc.gnu.org>
program p
  use, intrinsic :: ISO_FORTRAN_ENV, only: int64
  type t
    integer i
  end type
  type s
    class(t), allocatable :: c(:)
  end type
  integer :: rslt, class_rslt
  integer(kind=int64), target :: tgt
  class(t), allocatable, target :: t_alloc(:)
  class(s), allocatable, target :: s_alloc(:)
  character(:), allocatable, target :: chr(:)
  class(*), pointer :: ptr_s, ptr_a(:)

  allocate (t_alloc(2), source=t(1))
  rslt = storage_size(t_alloc(1))      ! Scalar arg - the original testcase
  if (rslt .ne. 32) stop 1

  rslt = storage_size(t_alloc)         ! Array arg
  if (rslt .ne. 32) stop 2

  call pr100027

  allocate (s_alloc(2), source=s([t(1), t(2)]))
! This, of course, is processor dependent: gfortran gives 576, NAG 448
! and Intel 1216.
  class_rslt = storage_size(s_alloc)   ! Type with a class component
  ptr_s => s_alloc(2)
! However, the unlimited polymorphic result should be the same
  if (storage_size (ptr_s) .ne. class_rslt) stop 3
  ptr_a => s_alloc
  if (storage_size (ptr_a) .ne. class_rslt) stop 4

  rslt = storage_size(s_alloc(1)%c(2)) ! Scalar component arg
  if (rslt .ne. 32) stop 5

  rslt = storage_size(s_alloc(1)%c)    ! Scalar component of array arg
  if (rslt .ne. 32) stop 6

  ptr_s => tgt
  rslt = storage_size (ptr_s)          ! INTEGER(8) target
  if (rslt .ne. 64) stop 7

  allocate (chr(2), source = ["abcde", "fghij"])
  ptr_s => chr(2)
  rslt = storage_size (ptr_s)          ! CHARACTER(5) scalar
  if (rslt .ne. 40) stop 8

  ptr_a => chr
  rslt = storage_size (ptr_a)          ! CHARACTER(5) array
  if (rslt .ne. 40) stop 9

  deallocate (t_alloc, s_alloc, chr)   ! For valgrind check

contains

! Original testcase from José Rui Faustino de Sousa
  subroutine pr100027
    implicit none

    integer, parameter :: n = 11

    type :: foo_t
    end type foo_t

    type, extends(foo_t) :: bar_t
    end type bar_t

    class(*),     pointer :: apu(:)
    class(foo_t), pointer :: apf(:)
    class(bar_t), pointer :: apb(:)
    type(bar_t),   target :: atb(n)

    integer :: m

    apu => atb
    m = storage_size(apu)
    if (m .ne. 0) stop 10
    apf => atb
    m = storage_size(apf)
    if (m .ne. 0) stop 11
    apb => atb
    m = storage_size(apb)
    if (m .ne. 0) stop 12
  end
end program p
