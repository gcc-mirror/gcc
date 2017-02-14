! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

! Contributed by Damian Rouson
! Check the new _caf_send_by_ref()-routine.

program main

implicit none

type :: mytype
  integer :: i
  integer, allocatable :: indices(:)
  real, dimension(2,5,3) :: volume
  integer, allocatable :: scalar
  integer :: j
  integer, allocatable :: matrix(:,:)
  real, allocatable :: dynvol(:,:,:)
end type

type arrtype
  type(mytype), allocatable :: vec(:)
  type(mytype), allocatable :: mat(:,:)
end type arrtype

type(mytype), save :: object[*]
type(arrtype), save :: bar[*]
integer :: i,j,me,neighbor
integer :: idx(5)
real, allocatable :: volume(:,:,:), vol2(:,:,:)
real :: vol_static(2,5,3)

idx = (/ 1,2,1,7,5 /)

me=this_image()
neighbor = merge(1,me+1,me==num_images())
object[neighbor]%indices=[(i,i=1,5)]
object[neighbor]%i = 37
object[neighbor]%scalar = 42
vol_static = reshape([(i, i=1, 2*5*3)], [2, 5, 3])
object[neighbor]%volume = vol_static
object[neighbor]%matrix = reshape([(i, i=1, 70)], [10, 7])
object[neighbor]%dynvol = vol_static
sync all
if (object%scalar /= 42) call abort()
if (any( object%indices /= [1,2,3,4,5] )) call abort()
if (any( object%matrix /= reshape([(i, i=1, 70)], [10, 7]))) call abort()
if (any( object%volume /= vol_static)) call abort()
if (any( object%dynvol /= vol_static)) call abort()

vol2 = vol_static
vol2(:, ::2, :) = 42
object[neighbor]%volume(:, ::2, :) = 42
object[neighbor]%dynvol(:, ::2, :) = 42
if (any( object%volume /= vol2)) call abort()
if (any( object%dynvol /= vol2)) call abort()

allocate(bar%vec(-2:2))

bar[neighbor]%vec(1)%volume = vol_static
if (any(bar%vec(1)%volume /= vol_static)) call abort()

i = 15
bar[neighbor]%vec(1)%scalar = i
if (.not. allocated(bar%vec(1)%scalar)) call abort()
if (bar%vec(1)%scalar /= 15) call abort()

bar[neighbor]%vec(0)%scalar = 27
if (.not. allocated(bar%vec(0)%scalar)) call abort()
if (bar%vec(0)%scalar /= 27) call abort()

bar[neighbor]%vec(1)%indices = [ 3, 4, 15 ]
allocate(bar%vec(2)%indices(5))
bar[neighbor]%vec(2)%indices = 89

if (.not. allocated(bar%vec(1)%indices)) call abort()
if (allocated(bar%vec(-2)%indices)) call abort()
if (allocated(bar%vec(-1)%indices)) call abort()
if (allocated(bar%vec( 0)%indices)) call abort()
if (.not. allocated(bar%vec( 2)%indices)) call abort()
if (any(bar%vec(2)%indices /= 89)) call abort()

if (any (bar%vec(1)%indices /= [ 3,4,15])) call abort()
end program
