! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR 27784 - Different strings should compare unequal even if they
!            have CHAR(0) in them.

      program main
      character*3 str1, str2
      call setval(str1, str2)
      if (str1 == str2) STOP 1
      end

      subroutine setval(str1, str2)
      character*3 str1, str2
      str1 = 'a' // CHAR(0) // 'a'
      str2 = 'a' // CHAR(0) // 'c'
      end
