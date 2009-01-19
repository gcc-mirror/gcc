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
       if (any(ida /= (/0,2/))) call abort

       ida = lbound(a%i)
       if (any(ida /= (/1,1/))) call abort

       ida = ubound(a)
       if (any(ida /= (/5,8/))) call abort
       
       ida = ubound(a%i)
       if (any(ida /= (/6,7/))) call abort

       end
