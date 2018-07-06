! { dg-do run }
! { dg-options "-std=f2003 " }
! PR65596 Namelist reads too far.
integer ,parameter :: CL=80
integer ,parameter :: AL=4

character(CL) :: mode
character(CL) :: cats(AL)
character(CL) :: dogs(AL)
character(CL) :: rslt(AL)
integer       :: ierr, k

namelist / theList / cats, dogs, mode

open(27,status="scratch")

write(27,'(A)')  "&theList"
write(27,'(A)')  " mode      = 'on'"
write(27,'(A)')  " dogs      = 'Rover',"
write(27,'(A)')  "             'Spot'"
write(27,'(A)')  " cats      = 'Fluffy',"
write(27,'(A)')  "             'Hairball'"
write(27,'(A)') "/"
rewind(27)

mode    = 'off'
cats(:) = '________'
dogs(:) = '________'

read (27, nml=theList, iostat=ierr)

if (ierr .ne. 0) STOP 1

rslt = ['Rover   ','Spot    ','________','________']
if (any(dogs.ne.rslt)) STOP 2

rslt = ['Fluffy  ','Hairball','________','________']
if (any(cats.ne.rslt)) STOP 3

close(27)

contains

subroutine abort()
  close(27)
  stop 500
end subroutine abort

end
