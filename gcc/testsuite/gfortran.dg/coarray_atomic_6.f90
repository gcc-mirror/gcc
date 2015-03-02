! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Contributed by Reinhold Bader
!
!
program def_and_ref
! compile only
  use, intrinsic :: iso_fortran_env
  implicit none
  type :: e
    integer(kind=atomic_int_kind) :: ia = 0
    logical(kind=atomic_logical_kind) :: la = .false.
  end type 

  type(e) :: a[*]
  
  integer :: ival = 0
  logical :: lval = .false.

  if (this_image() == 1) then
    call atomic_define(a[num_images()]%ia, 4)
    call atomic_define(a[num_images()]%la, .true.)
  end if
  if (this_image() == num_images()) then
    do while (ival == 0 .or. .not. lval)
       call atomic_ref(ival, a%ia)
       call atomic_ref(lval, a%la)
    end do
    if (ival == 4 .and. lval) then
      write(*,*) 'OK'
    else
      write(*,*) 'FAIL: ival,lval =', ival, lval
    end if
  end if
end program
