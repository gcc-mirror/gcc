! { dg-do run }
!
! PR libfortran/20660 and other bugs (not filed in bugzilla) relating
! to negative units
! PR 33055 Runtime error in INQUIRE unit existance with -fdefault-integer-8
! Test case update by Jerry DeLisle <jvdelisle@gcc.gnu.org>
!
! Bugs submitted by Walt Brainerd
      integer i,j
      logical l
      
      i = -1
! gfortran created a 'fort.-1' file and wrote "Hello" in it
      write (unit=i, fmt=*, iostat=j) "Hello"
      if (j <= 0) call abort
      
      i = -11
      open (unit=i, file="xxx", iostat=j)
      if (j <= 0) call abort

      i = -42
      inquire (unit=i, exist=l)
      if (l) call abort
      end
