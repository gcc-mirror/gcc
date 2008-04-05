! { dg-do compile }
! { dg-options "-std=f2003" }
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>

integer :: istat
character(25) :: msg
real, dimension(10) :: a, b
namelist /mynml/ a, b
msg = "null"
a = 43.21
WRITE(99,'(10f8.3)',decimal="comma") a
rewind(99)
read(99,'(dc,10f8.3)',blank=msg) b
write(99,'(dp,10f8.3)',round="up") ! { dg-error "not implemented" }
rewind(99)
read(99,'(10f8.3)',pad="yes")
msg="suppress"
write(99,'(10f8.3)',sign=msg)
write(99,delim="apostrophe", fmt=*)
write(99,nml=mynml,delim="none")
end
