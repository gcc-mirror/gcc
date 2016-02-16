! { dg-do run }
program namelist_delim_none
   implicit none
   character(512) :: internal_unit
   character(5), dimension(5) :: mystring
   real, dimension(4) :: somenum
   integer :: i
   namelist /mylist/ mystring, somenum
   mystring(1)='mon'
   mystring(2)='tue'
   mystring(3)='wed'
   mystring(4)='thu'
   mystring(5)='fri'
   somenum = reshape(source = (/ 2, 3, 5, 7 /), shape=shape(somenum))

   open(unit=10,status='scratch',delim='none')
   write(10, mylist)
   rewind(10)
   mystring = "xxxxx"
   rewind(10)
   do i=1,5
     read(10,'(a)') internal_unit
     if (i.eq.2 .and. internal_unit .ne. " MYSTRING=mon  tue  wed  thu  fri  ,") call abort
     if (scan(internal_unit,"""'").ne.0) print *, internal_unit
   end do
   close(10)
end program
