! { dg-do run }
! Testcase for the GNU extension OPEN(...,ACCESS="APPEND")
  open (10,err=900,access="append",position="asis") ! { dg-output ".*Extension.*" }
  call abort
 900 end
