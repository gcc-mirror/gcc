! Test acceptance of keywords in free format
! Origin: David Billinghurst <David.Billinghurst@riotinto.com>
!
! { dg-do compile }
! { dg-options "-ffree-form" }
  integer i, j
  i = 1
  if ( i .eq. 1 ) then
    go = 2
  endif
  if ( i .eq. 3 ) then
     i = 4
  end if
  do i = 1, 3
    j = i
  end do
  do j = 1, 3
    i = j
  enddo
  end
