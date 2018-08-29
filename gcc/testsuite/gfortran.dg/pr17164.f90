! { dg-do run }
! { dg-options "-std=legacy" }
!
! pr17164
! index aborts when substring is longer than string
      implicit none
      character*5 x
      integer i
      x='12345'
      i=index(x,'blablabl')
      if (i.ne.0) STOP 1
      end

