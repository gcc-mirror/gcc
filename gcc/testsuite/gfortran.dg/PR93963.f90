! { dg-do run }
!
! Test the fix for PR93963
!

module m
contains
function rank_p(this) result(rnk) bind(c)
  use, intrinsic :: iso_c_binding, only: c_int

  implicit none
  
  integer(kind=c_int), pointer, intent(in) :: this(..)
  integer(kind=c_int)                      :: rnk

  select rank(this)
  rank(0)
    rnk = 0
  rank(1)
    rnk = 1
  rank(2)
    rnk = 2
  rank(3)
    rnk = 3
  rank(4)
    rnk = 4
  rank(5)
    rnk = 5
  rank(6)
    rnk = 6
  rank(7)
    rnk = 7
  rank(8)
    rnk = 8
  rank(9)
    rnk = 9
  rank(10)
    rnk = 10
  rank(11)
    rnk = 11
  rank(12)
    rnk = 12
  rank(13)
    rnk = 13
  rank(14)
    rnk = 14
  rank(15)
    rnk = 15
  rank default
    rnk = -1000
  end select
  return
end function rank_p

function rank_a(this) result(rnk) bind(c)
  use, intrinsic :: iso_c_binding, only: c_int

  implicit none
  
  integer(kind=c_int), allocatable, intent(in) :: this(..)
  integer(kind=c_int)                          :: rnk

  select rank(this)
  rank(0)
    rnk = 0
  rank(1)
    rnk = 1
  rank(2)
    rnk = 2
  rank(3)
    rnk = 3
  rank(4)
    rnk = 4
  rank(5)
    rnk = 5
  rank(6)
    rnk = 6
  rank(7)
    rnk = 7
  rank(8)
    rnk = 8
  rank(9)
    rnk = 9
  rank(10)
    rnk = 10
  rank(11)
    rnk = 11
  rank(12)
    rnk = 12
  rank(13)
    rnk = 13
  rank(14)
    rnk = 14
  rank(15)
    rnk = 15
  rank default
    rnk = -1000
  end select
  return
end function rank_a

function rank_o(this) result(rnk) bind(c)
  use, intrinsic :: iso_c_binding, only: c_int

  implicit none
  
  integer(kind=c_int), intent(in) :: this(..)
  integer(kind=c_int)             :: rnk

  select rank(this)
  rank(0)
    rnk = 0
  rank(1)
    rnk = 1
  rank(2)
    rnk = 2
  rank(3)
    rnk = 3
  rank(4)
    rnk = 4
  rank(5)
    rnk = 5
  rank(6)
    rnk = 6
  rank(7)
    rnk = 7
  rank(8)
    rnk = 8
  rank(9)
    rnk = 9
  rank(10)
    rnk = 10
  rank(11)
    rnk = 11
  rank(12)
    rnk = 12
  rank(13)
    rnk = 13
  rank(14)
    rnk = 14
  rank(15)
    rnk = 15
  rank default
    rnk = -1000
  end select
  return
end function rank_o

end module m

program selr_p
  use m
  use, intrinsic :: iso_c_binding, only: c_int

  implicit none

  integer(kind=c_int), parameter :: siz = 7
  integer(kind=c_int), parameter :: rnk = 1

  integer(kind=c_int),     pointer :: intp(:)
  integer(kind=c_int), allocatable :: inta(:)
  integer(kind=c_int)              :: irnk

  nullify(intp)
  irnk = rank_p(intp)
  if (irnk /= rnk)        stop 1
  if (irnk /= rank(intp)) stop 2
  !
  irnk = rank_a(inta)
  if (irnk /= rnk)        stop 3
  if (irnk /= rank(inta)) stop 4
  !
  allocate(intp(siz))
  irnk = rank_p(intp)
  if (irnk /= rnk)        stop 5
  if (irnk /= rank(intp)) stop 6
  irnk = rank_o(intp)
  if (irnk /= rnk)        stop 7
  if (irnk /= rank(intp)) stop 8
  deallocate(intp)
  nullify(intp)
  !
  allocate(inta(siz))
  irnk = rank_a(inta)
  if (irnk /= rnk)        stop 9
  if (irnk /= rank(inta)) stop 10
  irnk = rank_o(inta)
  if (irnk /= rnk)        stop 11
  if (irnk /= rank(inta)) stop 12
  deallocate(inta)

end program selr_p
