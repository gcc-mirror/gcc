!{ dg-do run { target fd_truncate } }
!{ dg-options "-std=legacy" }
!
! Tests filling arrays from a namelist read when object list is not complete.
! Developed from a test case provided by Christoph Jacob.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>.
program pr24794

   implicit none
   integer, parameter :: maxop=15, iunit=7
   character*8 namea(maxop), nameb(maxop)
   integer i, ier

   namelist/ccsopr/ namea,nameb
   namea=""
   nameb=""
   open (12, status="scratch", delim="apostrophe")
   write (12, '(a)') "&ccsopr"
   write (12, '(a)') "    namea='spi01h','spi02o','spi03h','spi04o','spi05h',"
   write (12, '(a)') "          'spi07o','spi08h','spi09h',"
   write (12, '(a)') "    nameb='spi01h','spi03h','spi05h','spi06h','spi08h',"
   write (12, '(a)') "&end"

   rewind (12)
   read (12, nml=ccsopr, iostat=ier)
   if (ier.ne.0) STOP 1
   
   rewind (12)
   write(12,nml=ccsopr)
   
   rewind (12)
   read (12, nml=ccsopr, iostat=ier)
   if (ier.ne.0) STOP 2
   
   if (namea(2).ne."spi02o  ") STOP 3
   if (namea(9).ne."        ") STOP 4
   if (namea(15).ne."        ") STOP 5
   if (nameb(1).ne."spi01h  ") STOP 6
   if (nameb(6).ne."        ") STOP 7
   if (nameb(15).ne."        ") STOP 8
   
   close (12)
end program pr24794
