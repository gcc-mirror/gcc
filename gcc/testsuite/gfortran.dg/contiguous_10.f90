! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/91640
!
! Based on G. Steinmetz's test case
!
program p
   implicit none (type, external)
   real, target :: z(3) = 1.0
   real :: res(3)
   real, pointer :: xxx(:)

   res = 42.0
   call sub (-z, res)
   if (any (abs (res - (-1.0)) > epsilon(res))) stop 1
   if (any (abs (z - 1.0) > epsilon(z))) stop 2

   res = 43.0
   call sub (z*2.0, res)
   if (any (abs (res - 2.0) > epsilon(res))) stop 3
   if (any (abs (z - 1.0) > epsilon(z))) stop 4

   res = 44.0
   call sub(get_var(), res)
   if (any (abs (res - 1.0) > epsilon(res))) stop 5
   if (any (abs (z - 1.0) > epsilon(z))) stop 6

   call double(get_var())
   if (any (abs (z - 2.0) > epsilon(z))) stop 7

   call double(get_var_cont())
   if (any (abs (z - 4.0) > epsilon(z))) stop 8

   ! For cross check for copy-out:
   xxx => z
   if (any (abs (z - 4.0) > epsilon(z))) stop 10
   if (any (abs (xxx - 4.0) > epsilon(z))) stop 11
   call double (xxx)
   if (any (abs (z - 8.0) > epsilon(z))) stop 12
   if (any (abs (xxx - 8.0) > epsilon(z))) stop 13

contains
   subroutine sub (x, res)
      real, contiguous :: x(:)
      real :: res(3)
      res = x
   end
   subroutine double (x)
      real, contiguous :: x(:)
      x = x * 2.0
   end
   function get_var()
     real, pointer :: get_var(:)
     get_var => z
   end
   function get_var_cont()
     real, pointer, contiguous :: get_var_cont(:)
     get_var_cont => z
   end
end

! only 'xxx' should have a copy out:
! { dg-final { scan-tree-dump-times "D\\.\[0-9\].* = .*atmp\\.\[0-9\]*\\.data" 1 "original" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\].*xxx\\.span.* = .*atmp\\.\[0-9\]*\\.data" 1 "original" } }

! Only once 'z... = ' – for:   static real(kind=4) z[3] = {[0 ... 2]=1.0e+0};
! but don't match '(si)ze'
! { dg-final { scan-tree-dump-times "z\[^e\].* = " 1 "original" } }
