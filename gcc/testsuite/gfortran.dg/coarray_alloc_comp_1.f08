! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

! Contributed by Damian Rouson
! Check the new _caf_get_by_ref()-routine.

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
object%indices=[(i,i=1,5)]
allocate(object%scalar, object%matrix(10,7))
object%i = 37
object%scalar = 42
vol_static = reshape([(i, i=1, 2*5*3)], [2, 5, 3])
object%volume = vol_static
object%matrix = reshape([(i, i=1, 70)], [10, 7])
object%dynvol = vol_static
sync all
neighbor = merge(1,neighbor,me==num_images())
if (object[neighbor]%scalar /= 42) call abort()
if (object[neighbor]%indices(4) /= 4) call abort()
if (object[neighbor]%matrix(3,6) /= 53) call abort()
if (any( object[neighbor]%indices(:) /= [1,2,3,4,5] )) call abort()
if (any( object[neighbor]%matrix(:,:) /= reshape([(i, i=1, 70)], [10, 7]))) call abort()
if (any( object[neighbor]%matrix(3,:) /= [(i * 10 + 3, i=0, 6)])) call abort()
if (any( object[neighbor]%matrix(:,2) /= [(i + 10, i=1, 10)])) call abort()
if (any( object[neighbor]%matrix(idx,2) /= [11, 12, 11, 17, 15])) call abort()
if (any( object[neighbor]%matrix(3,idx) /= [3, 13, 3, 63, 43])) call abort()
if (any( object[neighbor]%matrix(2:8:4, 5:1:-1) /= reshape([42, 46, 32, 36, 22, 26, 12, 16, 2, 6], [2,5]))) call abort()
if (any( object[neighbor]%matrix(:8:4, 2::2) /= reshape([11, 15, 31, 35, 51, 55], [2,3]))) call abort()
if (any( object[neighbor]%volume /= vol_static)) call abort()
if (any( object[neighbor]%dynvol /= vol_static)) call abort()
if (any( object[neighbor]%volume(:, 2:4, :) /= vol_static(:, 2:4, :))) call abort()
if (any( object[neighbor]%dynvol(:, 2:4, :) /= vol_static(:, 2:4, :))) call abort()

vol2 = vol_static(:, ::2, :)
if (any( object[neighbor]%volume(:, ::2, :) /= vol2)) call abort()
if (any( object[neighbor]%dynvol(:, ::2, :) /= vol2)) call abort()

allocate(bar%vec(-2:2))

bar%vec(1)%volume = vol_static
if (any(bar[neighbor]%vec(1)%volume /= vol_static)) call abort()

i = 15
bar%vec(1)%scalar = i
if (.not. allocated(bar%vec(1)%scalar)) call abort()
if (bar[neighbor]%vec(1)%scalar /= 15) call abort()

bar%vec(0)%scalar = 27
if (.not. allocated(bar%vec(0)%scalar)) call abort()
if (bar[neighbor]%vec(0)%scalar /= 27) call abort()

bar%vec(1)%indices = [ 3, 4, 15 ]
allocate(bar%vec(2)%indices(5))
bar%vec(2)%indices = 89

if (.not. allocated(bar%vec(1)%indices)) call abort()
if (allocated(bar%vec(-2)%indices)) call abort()
if (allocated(bar%vec(-1)%indices)) call abort()
if (allocated(bar%vec( 0)%indices)) call abort()
if (.not. allocated(bar%vec( 2)%indices)) call abort()
if (any(bar[me]%vec(2)%indices /= 89)) call abort()

if (any (bar[neighbor]%vec(1)%indices /= [ 3,4,15])) call abort()

deallocate(bar%vec(2)%indices, object%scalar, object%matrix)
deallocate(bar%vec)
end program
