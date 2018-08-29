! { dg-do run }
!
! Testcase contributed by Andre Vehreschild  <vehre@gcc.gnu.org>

module module_finalize_29
  implicit none

  ! The type name is encoding the state of its finalizer being
  ! elemental (second letter 'e'), or non-element (second letter 'n')
  ! or array shaped (second letter 'a'), or shape-specific routine
  ! (generic; second letter 'g'),
  ! and whether the init-routine is elemental or not (third letter
  ! either 'e' or 'n').
  type ten
    integer :: i = 40
  contains
    final :: ten_fin
  end type ten

  type tee
    integer :: i = 41
  contains
    final :: tee_fin
  end type tee

  type tne
    integer :: i = 42
  contains
    final :: tne_fin
  end type tne

  type tnn
    integer :: i = 43
  contains
    final :: tnn_fin
  end type tnn

  type tae
    integer :: i = 44
  contains
    final :: tae_fin
  end type tae

  type tan
    integer :: i = 45
  contains
    final :: tan_fin
  end type tan

  type tge
    integer :: i = 46
  contains
    final :: tge_scalar_fin, tge_array_fin
  end type tge

  type tgn
    integer :: i = 47
  contains
    final :: tgn_scalar_fin, tgn_array_fin
  end type tgn

  integer :: ten_fin_counts, tee_fin_counts, tne_fin_counts, tnn_fin_counts
  integer :: tae_fin_counts, tan_fin_counts
  integer :: tge_scalar_fin_counts, tge_array_fin_counts
  integer :: tgn_scalar_fin_counts, tgn_array_fin_counts
contains
  impure elemental subroutine ten_fin(x)
    type(ten), intent(inout) :: x
    x%i = -10 * x%i
    ten_fin_counts = ten_fin_counts + 1
  end subroutine ten_fin

  impure elemental subroutine tee_fin(x)
    type(tee), intent(inout) :: x
    x%i = -11 * x%i
    tee_fin_counts = tee_fin_counts + 1
  end subroutine tee_fin

  subroutine tne_fin(x)
    type(tne), intent(inout) :: x
    x%i = -12 * x%i
    tne_fin_counts = tne_fin_counts + 1
  end subroutine tne_fin

  subroutine tnn_fin(x)
    type(tnn), intent(inout) :: x
    x%i = -13 * x%i
    tnn_fin_counts = tnn_fin_counts + 1
  end subroutine tnn_fin

  subroutine tae_fin(x)
    type(tae), intent(inout) :: x(:,:)
    x%i = -14 * x%i
    tae_fin_counts = tae_fin_counts + 1
  end subroutine tae_fin

  subroutine tan_fin(x)
    type(tan), intent(inout) :: x(:,:)
    x%i = -15 * x%i
    tan_fin_counts = tan_fin_counts + 1
  end subroutine tan_fin

  subroutine tge_scalar_fin(x)
    type(tge), intent(inout) :: x
    x%i = -16 * x%i
    tge_scalar_fin_counts = tge_scalar_fin_counts + 1
  end subroutine tge_scalar_fin

  subroutine tge_array_fin(x)
    type(tge), intent(inout) :: x(:,:)
    x%i = -17 * x%i
    tge_array_fin_counts = tge_array_fin_counts + 1
  end subroutine tge_array_fin

  subroutine tgn_scalar_fin(x)
    type(tgn), intent(inout) :: x
    x%i = -18 * x%i
    tgn_scalar_fin_counts = tgn_scalar_fin_counts + 1
  end subroutine tgn_scalar_fin

  subroutine tgn_array_fin(x)
    type(tgn), intent(inout) :: x(:,:)
    x%i = -19 * x%i
    tgn_array_fin_counts = tgn_array_fin_counts + 1
  end subroutine tgn_array_fin

  ! The finalizer/initializer call producer
  subroutine ten_init(x)
    class(ten), intent(out) :: x(:,:)
  end subroutine ten_init

  impure elemental subroutine tee_init(x)
    class(tee), intent(out) :: x
  end subroutine tee_init

  impure elemental subroutine tne_init(x)
    class(tne), intent(out) :: x
  end subroutine tne_init

  subroutine tnn_init(x)
    class(tnn), intent(out) :: x(:,:)
  end subroutine tnn_init

  impure elemental subroutine tae_init(x)
    class(tae), intent(out) :: x
  end subroutine tae_init

  subroutine tan_init(x)
    class(tan), intent(out) :: x(:,:)
  end subroutine tan_init

  impure elemental subroutine tge_init(x)
    class(tge), intent(out) :: x
  end subroutine tge_init

  subroutine tgn_init(x)
    class(tgn), intent(out) :: x(:,:)
  end subroutine tgn_init
end module module_finalize_29

program finalize_29
  use module_finalize_29
  implicit none

  type(ten), allocatable :: x_ten(:,:)
  type(tee), allocatable :: x_tee(:,:)
  type(tne), allocatable :: x_tne(:,:)
  type(tnn), allocatable :: x_tnn(:,:)
  type(tae), allocatable :: x_tae(:,:)
  type(tan), allocatable :: x_tan(:,:)
  type(tge), allocatable :: x_tge(:,:)
  type(tgn), allocatable :: x_tgn(:,:)

  ! Set the global counts to zero.
  ten_fin_counts = 0
  tee_fin_counts = 0
  tne_fin_counts = 0
  tnn_fin_counts = 0
  tae_fin_counts = 0
  tan_fin_counts = 0
  tge_scalar_fin_counts = 0
  tge_array_fin_counts = 0
  tgn_scalar_fin_counts = 0
  tgn_array_fin_counts = 0

  allocate(ten :: x_ten(5,5))
  allocate(tee :: x_tee(5,5))
  allocate(tne :: x_tne(5,5))
  allocate(tnn :: x_tnn(5,5))
  allocate(tae :: x_tae(5,5))
  allocate(tan :: x_tan(5,5))
  allocate(tge :: x_tge(5,5))
  allocate(tgn :: x_tgn(5,5))

  x_ten%i = 1
  x_tee%i = 2
  x_tne%i = 3
  x_tnn%i = 4
  x_tae%i = 5
  x_tan%i = 6
  x_tge%i = 7
  x_tgn%i = 8

  call ten_init(x_ten(::2, ::3))

  if (ten_fin_counts /= 6) STOP 1
  if (tee_fin_counts + tne_fin_counts + tnn_fin_counts + tae_fin_counts + &
        tan_fin_counts + tge_scalar_fin_counts + tge_array_fin_counts + &
        tgn_scalar_fin_counts + tgn_array_fin_counts /= 0) STOP 2
  ten_fin_counts = 0

  call tee_init(x_tee(::2, ::3))

  if (tee_fin_counts /= 6) STOP 3
  if (ten_fin_counts + tne_fin_counts + tnn_fin_counts + tae_fin_counts + &
        tan_fin_counts + tge_scalar_fin_counts + tge_array_fin_counts + &
        tgn_scalar_fin_counts + tgn_array_fin_counts /= 0) STOP 4
  tee_fin_counts = 0

  call tne_init(x_tne(::2, ::3))

  if (tne_fin_counts /= 6) STOP 5
  if (ten_fin_counts + tee_fin_counts + tnn_fin_counts + tae_fin_counts + &
        tan_fin_counts + tge_scalar_fin_counts + tge_array_fin_counts + &
        tgn_scalar_fin_counts + tgn_array_fin_counts /= 0) STOP 6
  tne_fin_counts = 0

  call tnn_init(x_tnn(::2, ::3))

  if (tnn_fin_counts /= 0) STOP 7
  if (ten_fin_counts + tee_fin_counts + tne_fin_counts + tae_fin_counts + &
        tan_fin_counts + tge_scalar_fin_counts + tge_array_fin_counts + &
        tgn_scalar_fin_counts + tgn_array_fin_counts /= 0) STOP 8

  call tae_init(x_tae(::2, ::3))

  if (tae_fin_counts /= 0) STOP 9
  if (ten_fin_counts + tee_fin_counts + tne_fin_counts + tnn_fin_counts + &
        tan_fin_counts + tge_scalar_fin_counts + tge_array_fin_counts + &
        tgn_scalar_fin_counts + tgn_array_fin_counts /= 0) STOP 10

  call tan_init(x_tan(::2, ::3))

  if (tan_fin_counts /= 1) STOP 11
  if (ten_fin_counts + tee_fin_counts + tne_fin_counts + tnn_fin_counts + &
        tae_fin_counts + tge_scalar_fin_counts + tge_array_fin_counts + &
        tgn_scalar_fin_counts + tgn_array_fin_counts /= 0) STOP 12
  tan_fin_counts = 0

  call tge_init(x_tge(::2, ::3))

  if (tge_scalar_fin_counts /= 6) STOP 13
  if (ten_fin_counts + tee_fin_counts + tne_fin_counts + tnn_fin_counts + &
        tae_fin_counts + tan_fin_counts + tgn_array_fin_counts + &
        tgn_scalar_fin_counts + tgn_array_fin_counts /= 0) STOP 14
  tge_scalar_fin_counts = 0

  call tgn_init(x_tgn(::2, ::3))

  if (tgn_array_fin_counts /= 1) STOP 15
  if (ten_fin_counts + tee_fin_counts + tne_fin_counts + tnn_fin_counts + &
        tae_fin_counts + tan_fin_counts + tge_scalar_fin_counts + &
        tge_array_fin_counts + tgn_scalar_fin_counts /= 0) STOP 16
  tgn_array_fin_counts = 0

  if (any (reshape (x_ten%i, [25]) /= [[40, 1, 40, 1, 40], [1, 1, 1, 1, 1],&
        [1, 1, 1, 1, 1], [40, 1, 40, 1, 40], [1, 1, 1, 1, 1]])) STOP 17

  if (any (reshape (x_tee%i, [25]) /= [[41, 2, 41, 2, 41], [2, 2, 2, 2, 2],&
        [2, 2, 2, 2, 2], [41, 2, 41, 2, 41], [2, 2, 2, 2, 2]])) STOP 18

  if (any (reshape (x_tne%i, [25]) /= [[42, 3, 42, 3, 42], [3, 3, 3, 3, 3],&
        [3, 3, 3, 3, 3], [42, 3, 42, 3, 42], [3, 3, 3, 3, 3]])) STOP 19

  if (any (reshape (x_tnn%i, [25]) /= [[43, 4, 43, 4, 43], [4, 4, 4, 4, 4],&
        [4, 4, 4, 4, 4], [43, 4, 43, 4, 43], [4, 4, 4, 4, 4]])) STOP 20

  if (any (reshape (x_tae%i, [25]) /= [[44, 5, 44, 5, 44], [5, 5, 5, 5, 5],&
        [5, 5, 5, 5, 5], [44, 5, 44, 5, 44], [5, 5, 5, 5, 5]])) STOP 21

  if (any (reshape (x_tan%i, [25]) /= [[45, 6, 45, 6, 45], [6, 6, 6, 6, 6],&
        [6, 6, 6, 6, 6], [45, 6, 45, 6, 45], [6, 6, 6, 6, 6]])) STOP 22

  if (any (reshape (x_tge%i, [25]) /= [[46, 7, 46, 7, 46], [7, 7, 7, 7, 7],&
        [7, 7, 7, 7, 7], [46, 7, 46, 7, 46], [7, 7, 7, 7, 7]])) STOP 23

  if (any (reshape (x_tgn%i, [25]) /= [[47, 8, 47, 8, 47], [8, 8, 8, 8, 8],&
        [8, 8, 8, 8, 8], [47, 8, 47, 8, 47], [8, 8, 8, 8, 8]])) STOP 24
end program finalize_29
