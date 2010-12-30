! { dg-do compile }
! { dg-options "" }
! (options to disable warnings about statement functions etc.)
!
! PR fortran/44352
!
! Contributed by Vittorio Zecca
!

      SUBROUTINE TEST1()
      implicit real*8 (a-h,o-z)
      character*32 ddname,stmtfnt1
      stmtfnt1(x)=   'h810 e=0.01         '
      ddname=stmtfnt1(0.d0)
      if (ddname /= "h810 e=0.01") call abort()
      END

      SUBROUTINE TEST2()
      implicit none
      character(2)  :: ddname,stmtfnt2
      real :: x
      stmtfnt2(x)=   'x'
      ddname=stmtfnt2(0.0)
      if(ddname /= 'x') call abort()
      END

      SUBROUTINE TEST3()
      implicit real*8 (a-h,o-z)
      character*32 ddname,dname
      character*2 :: c
      dname(c) = 'h810 e=0.01         '
      ddname=dname("w ")
      if (ddname /= "h810 e=0.01") call abort()
      END

      SUBROUTINE TEST4()
      implicit real*8 (a-h,o-z)
      character*32 ddname,dname
      character*2 :: c
      dname(c) = 'h810 e=0.01         '
      c = 'aa'
      ddname=dname("w ")
      if (ddname /= "h810 e=0.01") call abort()
      if (c /= "aa") call abort()
      END

      call test1()
      call test2()
      call test3()
      call test4()
      end
