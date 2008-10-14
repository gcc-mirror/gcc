! { dg-do run }
! PR28339, This test checks that internal unit array I/O handles a full record
! and advances to the next record properly.  Test case derived from PR
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      program main
      integer          i
      character*8      rec(3)
      rec = ""
      write (rec,fmt=99999)
      if (rec(1).ne.'12345678') call abort()
      if (rec(2).ne.'record2') call abort()
      if (rec(3).ne.'record3') call abort()
99999 format ('12345678',/'record2',/'record3')
      end

