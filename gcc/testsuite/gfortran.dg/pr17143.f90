! { dg-do run }
! { dg-options "-std=legacy" }
!
! pr17143
! does not print 2*63 correctly
       character*25 l
       integer(kind=8) i
       data i /1/
       do j = 1,63
          i = i * 2
       end do
       write(l,*)i
       if (l.ne.' -9223372036854775808') then
!                ^
!         the space is required before a number
          STOP 1
       endif 
       end

