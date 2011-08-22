! { dg-do compile }
!  PR 50130 - this caused an ICE.  Test case supplied by Joost
!  VandeVondele.
integer, parameter :: a(10)=0
integer, parameter :: b(10)=a(1:10:0) ! { dg-error "Illegal stride of zero" }
END

