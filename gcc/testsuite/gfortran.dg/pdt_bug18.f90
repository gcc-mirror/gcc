! { dg-do run }
program q1
  type :: t(n)
    integer, len :: n = 1
    integer :: v(n)
  end type
  type(t(5)) :: a
  namelist /grp/ a
  a%v = [1,2,3,4,5]
  open (10, status='SCRATCH')
  write (10, nml=grp)
  a%v = 0
  rewind (10)
  read (10, nml=grp)
  close (10)
  if (any (a%v /= [1,2,3,4,5])) stop 1
end program
