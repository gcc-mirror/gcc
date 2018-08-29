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
if (object%scalar /= 42) STOP 1
if (any( object%indices /= [1,2,3,4,5] )) STOP 2
if (any( object%matrix /= reshape([(i, i=1, 70)], [10, 7]))) STOP 3
if (any( object%volume /= vol_static)) STOP 4
if (any( object%dynvol /= vol_static)) STOP 5

vol2 = vol_static
vol2(:, ::2, :) = 42
object[neighbor]%volume(:, ::2, :) = 42
object[neighbor]%dynvol(:, ::2, :) = 42
if (any( object%volume /= vol2)) STOP 6
if (any( object%dynvol /= vol2)) STOP 7

allocate(bar%vec(-2:2))

bar[neighbor]%vec(1)%volume = vol_static
if (any(bar%vec(1)%volume /= vol_static)) STOP 8

i = 15
bar[neighbor]%vec(1)%scalar = i
if (.not. allocated(bar%vec(1)%scalar)) STOP 9
if (bar%vec(1)%scalar /= 15) STOP 10

bar[neighbor]%vec(0)%scalar = 27
if (.not. allocated(bar%vec(0)%scalar)) STOP 11
if (bar%vec(0)%scalar /= 27) STOP 12

bar[neighbor]%vec(1)%indices = [ 3, 4, 15 ]
allocate(bar%vec(2)%indices(5))
bar[neighbor]%vec(2)%indices = 89

if (.not. allocated(bar%vec(1)%indices)) STOP 13
if (allocated(bar%vec(-2)%indices)) STOP 14
if (allocated(bar%vec(-1)%indices)) STOP 15
if (allocated(bar%vec( 0)%indices)) STOP 16
if (.not. allocated(bar%vec( 2)%indices)) STOP 17
if (any(bar%vec(2)%indices /= 89)) STOP 18

if (any (bar%vec(1)%indices /= [ 3,4,15])) STOP 19
end program
