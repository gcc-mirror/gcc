! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program check_caf_send_by_ref

  implicit none

  type T
    integer, allocatable :: scal
    integer, allocatable :: array(:)
  end type T

  type(T), save :: obj[*]
  integer :: me, np, i

  me = this_image()
  np = num_images()

  obj[np]%scal = 42

  ! Check the token for the scalar is set.
  if (obj[np]%scal /= 42) STOP 1

  ! Now the same for arrays.
  obj[np]%array = [(i * np + me, i = 1, 15)]
  if (any(obj[np]%array /= [(i * np + me, i = 1, 15)])) STOP 2

end program check_caf_send_by_ref

