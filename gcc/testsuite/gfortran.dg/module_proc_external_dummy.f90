! { dg-do compile }
! This tests the fix for PR24866 in which the reference to the external str, in
! sub_module, would get mixed up with the module procedure, str, thus
! causing an ICE.  This is a completed version of the reporter's testcase; ie
! it adds a main program and working subroutines to allow a check for
! correct functioning.
!
! Contributed by Uttam Pawar  <uttamp@us.ibm.com>
!
   subroutine sub()
     print *, "external sub"
   end subroutine sub

module test_module
 contains
   subroutine sub_module(str)
     external :: str
     call str ()
   end subroutine sub_module
   subroutine str()
     print *, "module str"
   end subroutine str
end module test_module

   use test_module
   external sub
   call sub_module (sub)
   call sub_module (str)
end
