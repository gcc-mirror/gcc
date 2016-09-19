! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single -fdump-tree-original" }

program alloc_comp
  implicit none

  type coords
    real,allocatable :: x(:)
    real,allocatable :: y(:)
    real,allocatable :: z(:)
  end type

  integer :: me,np,n,i
  type(coords) :: coo[*]

  ! with caf_single num_images is always == 1
  me = this_image(); np = num_images()
  n = 100

  allocate(coo%x(n),coo%y(n),coo%z(n))

  coo%y = me

  do i=1, n
        coo%y(i) = coo%y(i) + i
  end do

  sync all

  ! Check the caf_get()-offset is computed correctly.
  if(me == 1 .and. coo[np]%y(10) /= 11 ) call abort()

  ! Check the whole array is correct.
  if (me == 1 .and. any( coo[np]%y /= [(i, i=2, 101)] ) ) call abort()

  deallocate(coo%x)

end program
