! { dg-do run }
! { dg-options "-funsigned -pedantic" }
program memain
  implicit none
  integer :: iostat
  character(len=100) :: iomsg
  unsigned :: u
  open (10)
  write (10,'(I10)') -1
  write (10,'(I10)') 2_8**32
  rewind 10
  read (10,'(I10)',iostat=iostat,iomsg=iomsg) u
  if (iostat == 0) error stop 1
  if (iomsg /= "Negative sign for unsigned integer read") error stop 2
  read (10,'(I10)',iostat=iostat,iomsg=iomsg) u
  if (iostat == 0) error stop 3
  if (iomsg /= "Value overflowed during unsigned integer read") error stop 4
  rewind 10
  read (10,*,iostat=iostat,iomsg=iomsg) u
  if (iostat == 0) error stop 5
  if (iomsg /= "Negative sign for unsigned integer in item 1 of list input ") error stop 6
  read (10,*,iostat=iostat,iomsg=iomsg) u
  if (iostat == 0) error stop 7
  if (iomsg /= "Unsigned integer overflow while reading item 1 of list input") error stop 8
  close(unit=10, status='delete')
 end program memain
