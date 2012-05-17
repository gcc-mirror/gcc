! { dg-do run }
! Tests the fix for PR41772 in which the empty array reference
! 'qname(1:n-1)' was not handled correctly in TRANSFER.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  implicit none
contains
  pure function str_vs(vs) result(s)
    character, dimension(:), intent(in) :: vs
    character(len=size(vs)) :: s
    s = transfer(vs, s)
  end function str_vs
  subroutine has_key_ns(uri, localname, n)
    character(len=*), intent(in) :: uri, localname
    integer, intent(in) :: n
    if ((n .lt. 2) .and. (len (uri) .ne. 0)) then
      call abort
    else IF ((n .ge. 2) .and. (len (uri) .ne. n - 1)) then 
      call abort
    end if
  end subroutine
end module m

  use m
  implicit none
  character, dimension(:), pointer :: QName
  integer :: n
  allocate(qname(6))
  qname = (/ 'a','b','c','d','e','f' /)

  do n = 0, 3
    call has_key_ns(str_vs(qname(1:n-1)),"", n)
  end do
  deallocate(qname)
end
