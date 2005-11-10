! { dg-do compile }
! PR 24643
! This tests a case where the compiler used to ICE in an early
! incarnation of the patch
ylocal=1
ybtable=ylocal(1:2)  ! { dg-error "Unclassifiable" }
end
