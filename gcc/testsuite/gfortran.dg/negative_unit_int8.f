! { dg-do run }
! { dg-options "-fdefault-integer-8" }
! 
! NOTE: This test is identical to negative_unit.f except -fdefault-integer-8
!
! PR libfortran/20660 and other bugs (not filed in bugzilla) relating
! to negative units
! PR 33055 Runtime error in INQUIRE unit existance with -fdefault-integer-8
! Test case update by Jerry DeLisle <jvdelisle@gcc.gnu.org>
!
! Bugs submitted by Walt Brainerd
      integer i
      integer, parameter ::ERROR_BAD_UNIT = 5005
      logical l
      
      i = -1
! gfortran created a 'fort.-1' file and wrote "Hello" in it
      write (unit=i, fmt=*, iostat=i) "Hello"
      if (i <= 0) STOP 1
      
      i = -11
      open (unit=i, file="xxx", iostat=i)
      if (i <= 0) STOP 2

      i = -42
      inquire (unit=i, exist=l)
      if (l) STOP 3

      i = 2_8*huge(0_4)+20_8
! This one is nasty
      inquire (unit=i, exist=l, iostat=i)
      if (l) STOP 4
      if (i.ne.0) STOP 5

      end
