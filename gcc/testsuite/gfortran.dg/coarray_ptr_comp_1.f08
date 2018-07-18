! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

! Contributed by Damian Rouson
! Check the new _caf_get_by_ref()-routine.
! Same like coarray_alloc_comp_1 but for pointers.

program main

implicit none

type :: mytype
  integer :: i
  integer, pointer :: indices(:)
  real, dimension(2,5,3) :: volume
  integer, pointer :: scalar
  integer :: j
  integer, pointer :: matrix(:,:)
  real, pointer :: dynvol(:,:,:)
end type

type arrtype
  type(mytype), pointer :: vec(:)
  type(mytype), pointer :: mat(:,:)
end type arrtype

type(mytype), save :: object[*]
type(arrtype), save :: bar[*]
integer :: i,j,me,neighbor
integer :: idx(5)
real, allocatable :: volume(:,:,:), vol2(:,:,:)
real, target :: vol_static(2,5,3)

idx = (/ 1,2,1,7,5 /)

me=this_image()
allocate(object%indices, source=[(i,i=1,5)])
allocate(object%scalar, object%matrix(10,7))
object%i = 37
object%scalar = 42
vol_static = reshape([(i, i=1, 2*5*3)], [2, 5, 3])
object%volume = vol_static
object%matrix = reshape([(i, i=1, 70)], [10, 7])
object%dynvol => vol_static
sync all
neighbor = merge(1,neighbor,me==num_images())
if (object[neighbor]%scalar /= 42) STOP 1
if (object[neighbor]%indices(4) /= 4) STOP 2
if (object[neighbor]%matrix(3,6) /= 53) STOP 3
if (any( object[neighbor]%indices(:) /= [1,2,3,4,5] )) STOP 4
if (any( object[neighbor]%matrix(:,:) /= reshape([(i, i=1, 70)], [10, 7]))) STOP 5
if (any( object[neighbor]%matrix(3,:) /= [(i * 10 + 3, i=0, 6)])) STOP 6
if (any( object[neighbor]%matrix(:,2) /= [(i + 10, i=1, 10)])) STOP 7
if (any( object[neighbor]%matrix(idx,2) /= [11, 12, 11, 17, 15])) STOP 8
if (any( object[neighbor]%matrix(3,idx) /= [3, 13, 3, 63, 43])) STOP 9
if (any( object[neighbor]%matrix(2:8:4, 5:1:-1) /= reshape([42, 46, 32, 36, 22, 26, 12, 16, 2, 6], [2,5]))) STOP 10
if (any( object[neighbor]%matrix(:8:4, 2::2) /= reshape([11, 15, 31, 35, 51, 55], [2,3]))) STOP 11
if (any( object[neighbor]%volume /= vol_static)) STOP 12
if (any( object[neighbor]%dynvol /= vol_static)) STOP 13
if (any( object[neighbor]%volume(:, 2:4, :) /= vol_static(:, 2:4, :))) STOP 14
if (any( object[neighbor]%dynvol(:, 2:4, :) /= vol_static(:, 2:4, :))) STOP 15

vol2 = vol_static(:, ::2, :)
if (any( object[neighbor]%volume(:, ::2, :) /= vol2)) STOP 16
if (any( object[neighbor]%dynvol(:, ::2, :) /= vol2)) STOP 17

allocate(bar%vec(-2:2))

bar%vec(1)%volume = vol_static
if (any(bar[neighbor]%vec(1)%volume /= vol_static)) STOP 18

i = 15
allocate(bar%vec(1)%scalar, bar%vec(0)%scalar)
bar%vec(1)%scalar = i
if (.not. associated(bar%vec(1)%scalar)) STOP 19
if (bar[neighbor]%vec(1)%scalar /= 15) STOP 20

bar%vec(0)%scalar = 27
if (.not. associated(bar%vec(0)%scalar)) STOP 21
if (bar[neighbor]%vec(0)%scalar /= 27) STOP 22

allocate(bar%vec(1)%indices(3), bar%vec(2)%indices(5))
bar%vec(1)%indices = [ 3, 4, 15 ]
bar%vec(2)%indices = 89

if (.not. associated(bar%vec(1)%indices)) STOP 23
if (associated(bar%vec(-2)%indices)) STOP 24
if (associated(bar%vec(-1)%indices)) STOP 25
if (associated(bar%vec( 0)%indices)) STOP 26
if (.not. associated(bar%vec( 2)%indices)) STOP 27
if (any(bar[me]%vec(2)%indices /= 89)) STOP 28

if (any (bar[neighbor]%vec(1)%indices /= [ 3,4,15])) STOP 29

deallocate(bar%vec(2)%indices, bar%vec(1)%indices, bar%vec(1)%scalar, bar%vec(0)%scalar)
deallocate(object%indices, object%scalar, object%matrix)
deallocate(bar%vec)
end program
