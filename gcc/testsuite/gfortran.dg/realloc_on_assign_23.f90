! { dg-do run }
!
! PR fortran/57354
!
! Contributed by Vladimir Fuka  <vladimir.fuka@gmail.com>
!
  type t
    integer,allocatable :: i
  end type

  type(t) :: e
  type(t), allocatable :: a(:)
  integer :: chksum = 0

  do i=1,3   ! Was 100 in original
    e%i = i
    chksum = chksum + i
    if (.not.allocated(a)) then
      a = [e]
    else
      call foo
    end if
  end do

  if (sum ([(a(i)%i, i=1,size(a))]) .ne. chksum) call abort
contains
  subroutine foo
    a = [a, e]
  end subroutine
end
