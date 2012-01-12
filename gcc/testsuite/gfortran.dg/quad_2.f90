! { dg-do run }
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
   if (fp1 /= fp3) call abort()
   read (str2, *) fp3
   if (fp1 /= fp3) call abort()
   read (str3, *) fp4
   if (fp2 /= fp4) call abort()
   read (str4, *) fp4
   if (fp2 /= fp4) call abort()

   select case (qp)
     case (8)
       if (str1 /= "   1.0000000000000000") call abort()
       if (str2 /= "1.0000000000000000") call abort()
       if (str3 /= "   1.4142135623730951") call abort()
       if (str4 /= "1.4142135623730951") call abort()

     case (10)
       if (str1 /= "   1.00000000000000000000") call abort()
       if (str2 /= "1.00000000000000000000") call abort()
       if (str3 /= "   1.41421356237309504876") call abort()
       if (str4 /= "1.41421356237309504876") call abort()

     case (16)
       if (str1 /= "   1.00000000000000000000000000000000000") call abort()
       if (str2 /= "1.00000000000000000000000000000000000") call abort()

       if (digits(1.0_qp) == 113) then
         ! IEEE 754 binary 128 format
         ! e.g. libquadmath/__float128 on i686/x86_64/ia64
         if (str3 /= "   1.41421356237309504880168872420969798") call abort()
         if (str4 /= "1.41421356237309504880168872420969798") call abort()
       else if (digits(1.0_qp) == 106) then
         ! IBM binary 128 format
         if (str3(1:37) /= "   1.41421356237309504880168872420969") call abort()
         if (str4(1:34) /= "1.41421356237309504880168872420969") call abort()
       end if

       ! Do a libm run-time test
       block
         real(qp), volatile :: fp2a
         fp2a = 2.0_qp
         fp2a = sqrt (fp2a)
         if (abs (fp2a - fp2) > sqrt(2.0_qp)-nearest(sqrt(2.0_qp),-1.0_qp)) call abort()
       end block

     case default
       call abort()
   end select

end program test_qp
