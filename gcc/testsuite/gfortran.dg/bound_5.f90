! { dg-do run }
!
! PR fortran/38859
! Wrong bounds simplification
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>

       type x
         integer I
       end type x
       type (x) A(0:5, 2:8)
       integer ida(2)

       ida = lbound(a)
       if (any(ida /= (/0,2/))) STOP 1

       ida = lbound(a%i)
       if (any(ida /= (/1,1/))) STOP 2

       ida = ubound(a)
       if (any(ida /= (/5,8/))) STOP 3
       
       ida = ubound(a%i)
       if (any(ida /= (/6,7/))) STOP 4

       end
