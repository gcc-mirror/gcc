! { dg-do run }
! { dg-options "-std=f2008 " }
!
! Passing a null pointer or deallocated variable to an
! optional, non-pointer, non-allocatable dummy.
!
program test
  implicit none
  integer, pointer :: ps => NULL(), pa(:) => NULL()
  integer, allocatable :: as, aa(:)

  call scalar(ps) 
  call scalar(as) 
  call scalar() 
  call scalar(NULL())

  call assumed_size(pa) 
  call assumed_size(aa) 
  call assumed_size() 
  call assumed_size(NULL(pa))

  call assumed_shape(pa)
  call assumed_shape(aa)
  call assumed_shape()
  call assumed_shape(NULL())

  call ptr_func(.true., ps)
  call ptr_func(.true., null())
  call ptr_func(.false.)
contains
  subroutine scalar(a)
    integer, optional :: a
    if (present(a)) STOP 1
  end subroutine scalar
  subroutine assumed_size(a)
    integer, optional :: a(*)
    if (present(a)) STOP 2
  end subroutine assumed_size
  subroutine assumed_shape(a)
    integer, optional :: a(:)
    if (present(a)) STOP 3
  end subroutine assumed_shape
  subroutine ptr_func(is_psnt, a)
    integer, optional, pointer :: a
    logical :: is_psnt
    if (is_psnt .neqv. present(a)) STOP 4
  end subroutine ptr_func
end program test
