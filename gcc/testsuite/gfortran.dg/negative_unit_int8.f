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
      
      i = 0
! gfortran created a 'fort.-1' file and wrote "Hello" in it
      write (unit=-1, fmt=*, iostat=i) "Hello"
      if (i <= 0) call abort
      
      i = 0
      open (unit=-11, file="xxx", iostat=i)
      if (i <= 0) call abort

      i = 0
      inquire (unit=-42, exist=l)
      if (l) call abort

      i = 0 
! This one is nasty
      inquire (unit=2_8*huge(0_4)+20_8, exist=l, iostat=i)
      if (l) call abort
      if (i.ne.ERROR_BAD_UNIT) call abort

      end
