! PR fortran/21730
! { dg-do run }
! { dg-options "-std=legacy" }
!
      character*2 a (1)
      character*4 b (1)
      character*6 c
      parameter (a="12")
      parameter (b = a)
      write (c,'("#",A,"#")') b
      if (c .ne. '#12  #') call abort
      end

