! { dg-do run }
! PR30005 Enhanced error messages for OPEN
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
character(60) :: msg
character(25) :: n = "temptestfile"
open(77,file=n,status="new")
close(77, status="keep")
msg=""
open(77,file=n,status="new", iomsg=msg, iostat=i)
if (msg /= "File 'temptestfile' already exists") call abort()

open(77,file=n,status="old")
close(77, status="delete")
open(77,file=n,status="old", iomsg=msg, iostat=i)
if (msg /= "File 'temptestfile' does not exist") call abort()

open(77,file="./", iomsg=msg, iostat=i)
if (msg /= "'./' is a directory") call abort()

open(77,file=n,status="new")
i = chmod(n, "-w")
if (i == 0) then
 close(77, status="keep")
 open(77,file=n, iomsg=msg, iostat=i, action="write")
 if (msg /= "Permission denied trying to open file 'temptestfile'") call abort()
endif

i = chmod(n,"+w")
open(77,file=n, iomsg=msg, iostat=i, action="read")
close(77, status="delete")
end
