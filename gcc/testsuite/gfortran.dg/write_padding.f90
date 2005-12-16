! { dg-do run }
! PR25264 Verify that the internal unit, str, is not cleared
! before it is needed elsewhere.  This is an extension.
! Test derived from test case by JPR. Contributed by
! Jerry DeLisle <jvdelisle@verizon.net>.
program write_padding
   character(len=10) :: str
   real :: atime
   str = '123'
   write( str, '(a3,i1)' ) trim(str),4
   if (str.ne."1234") call abort()
end program write_padding

