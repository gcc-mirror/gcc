! { dg-do run }
! Testcase for the GNU extension OPEN(...,ACCESS="APPEND")
  open (10,file="foo")
  close (10,status="delete")

  open (10,file="foo",access="append") ! { dg-warning "Extension: ACCESS specifier in OPEN statement" }
  write (10,*) 42
  close (10,status="keep")
  open (10,file="foo",access="append") ! { dg-warning "Extension: ACCESS specifier in OPEN statement" }
  write (10,*) -42
  close (10,status="keep")

  open (10,file="foo")
  read (10,*) i
  if (i /= 42) call abort
  read (10,*) i
  if (i /= -42) call abort
  close (10,status="delete")

  end
! { dg-output ".*Extension.*Extension" } 
