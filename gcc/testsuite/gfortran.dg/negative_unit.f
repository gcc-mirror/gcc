! { dg-do run }
!
! PR libfortran/20660 and other bugs (not filed in bugzilla) relating
! to negative units
!
! Bugs submitted by Walt Brainerd
      integer i
      logical l
      
      i = 0
! gfortran created a 'fort.-1' file and wrote "Hello" in it
      write (unit=-1, fmt=*, iostat=i) "Hello"
      if (i <= 0) call abort
      
      i = 0
      open (unit=-11, file="xxx", iostat=i)
      if (i <= 0) call abort

      inquire (unit=-42, exist=l)
      if (l) call abort

      end
