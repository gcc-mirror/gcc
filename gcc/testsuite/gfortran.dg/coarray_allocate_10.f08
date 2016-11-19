! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program alloc_comp
  implicit none

  type coords
    integer,allocatable :: x(:)
  end type

  type outerT
    type(coords),allocatable :: coo[:]
  end type
  integer :: me,np,n,i
  type(outerT) :: o

  ! with caf_single num_images is always == 1
  me = this_image(); np = num_images()
  n = 100

  allocate(o%coo[*])
  allocate(o%coo%x(n))

  o%coo%x = me

  do i=1, n
        o%coo%x(i) = o%coo%x(i) + i
  end do

  sync all

  if(me == 1 .and. o%coo[np]%x(10) /= 11 ) call abort()

  ! Check the whole array is correct.
  if (me == 1 .and. any( o%coo[np]%x /= [(i, i=2, 101)] ) ) call abort()

  deallocate(o%coo%x)

end program
