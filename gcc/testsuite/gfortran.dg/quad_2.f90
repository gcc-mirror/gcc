! { dg-do run { xfail hppa*-*-hpux* } }
! { dg-require-effective-target fortran_largest_fp_has_sqrt }
!
! This test checks whether the largest possible
! floating-point number works.
!
! This is a run-time check. Depending on the architecture,
! this tests REAL(8), REAL(10) or REAL(16) and REAL(16)
! might be a hardware or libquadmath 128bit number.
!
program test_qp
   use iso_fortran_env, only: real_kinds
   implicit none
   integer, parameter :: QP = real_kinds(ubound(real_kinds,dim=1))
   real(qp) :: fp1, fp2, fp3, fp4
   character(len=80) :: str1, str2, str3, str4
   fp1 = 1
   fp2 = sqrt (2.0_qp)
   write (str1,*) fp1
   write (str2,'(g0)') fp1
   write (str3,*) fp2
   write (str4,'(g0)') fp2

!   print '(3a)', '>',trim(str1),'<'
!   print '(3a)', '>',trim(str2),'<'
!   print '(3a)', '>',trim(str3),'<'
!   print '(3a)', '>',trim(str4),'<'

   read (str1, *) fp3
   if (fp1 /= fp3) STOP 1
   read (str2, *) fp3
   if (fp1 /= fp3) STOP 2
   read (str3, *) fp4
   if (abs (fp2 - fp4)/fp2 > epsilon(fp2)) STOP 3
   read (str4, *) fp4
   if (abs (fp2 - fp4)/fp2 > epsilon(fp2)) STOP 4

   select case (qp)
     case (8)
       if (str1 /= "   1.0000000000000000") STOP 5
       if (str2 /= "1.0000000000000000") STOP 6
       if (str3 /= "   1.4142135623730951") STOP 7
       if (str4 /= "1.4142135623730951") STOP 8

     case (10)
       if (str1 /= "   1.00000000000000000000") STOP 9
       if (str2 /= "1.00000000000000000000") STOP 10
       if (str3 /= "   1.41421356237309504876") STOP 11
       if (str4 /= "1.41421356237309504876") STOP 12

     case (16)
       if (digits(1.0_qp) == 113) then
         ! IEEE 754 binary 128 format
         ! e.g. libquadmath/__float128 on i686/x86_64/ia64
         if (str1 /= "   1.00000000000000000000000000000000000") STOP 13
         if (str2 /= "1.00000000000000000000000000000000000") STOP 14
         if (str3 /= "   1.41421356237309504880168872420969798") STOP 15
         if (str4 /= "1.41421356237309504880168872420969798") STOP 16
       else if (digits(1.0_qp) == 106) then
         ! IBM binary 128 format
         if (str1 /= "   1.0000000000000000000000000000000") STOP 17
         if (str2 /= "1.0000000000000000000000000000000") STOP 18
         if (str3(1:37) /= "   1.4142135623730950488016887242097") STOP 19
         if (str4(1:34) /= "1.4142135623730950488016887242097") STOP 20
       end if

       ! Do a libm run-time test
       block
         real(qp), volatile :: fp2a
         fp2a = 2.0_qp
         fp2a = sqrt (fp2a)
         if (abs (fp2a - fp2) > sqrt(2.0_qp)-nearest(sqrt(2.0_qp),-1.0_qp)) STOP 21
       end block

     case default
       STOP 22
   end select

end program test_qp
