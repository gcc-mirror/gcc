! { dg-do compile }
!
! PR fortran/38404 - location marker in wrong line
! Testcase contributed by Steve Chapel <steve DOT chapel AT a2pg DOT com>
!

      CHARACTER(len=72) TEXT(3)
      DATA (TEXT(I),I=1,3)/
     &'a string without issues',
     &'a string with too many characters properly broken into the next
     &line but too long to fit the variable',
     &                                                                '
     &a string that started just at the end of the last line -- some 
     &may not be helped'/

      ! { dg-warning "truncated" "" { target *-*-* } 10 }
      ! { dg-warning "truncated" "" { target *-*-* } 12 }

      END
