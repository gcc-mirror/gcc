!{ dg-do run }
! Tests filling arrays from a namelist read when object list is not complete.
! This is the same as namelist_21.f90 except using spaces as seperators instead
! of commas. Developed from a test case provided by Christoph Jacob.
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
   write (12, '(a)') "    namea='spi01h' 'spi02o' 'spi03h' 'spi04o' 'spi05h'"
   write (12, '(a)') "          'spi07o' 'spi08h' 'spi09h'"
   write (12, '(a)') "    nameb='spi01h' 'spi03h' 'spi05h' 'spi06h' 'spi08h'"
   write (12, '(a)') "&end"

   rewind (12)
   read (12, nml=ccsopr, iostat=ier)
   if (ier.ne.0) call abort()
   
   rewind (12)
   write(12,nml=ccsopr)
   
   rewind (12)
   read (12, nml=ccsopr, iostat=ier)
   if (ier.ne.0) call abort()
   if (namea(2).ne."spi02o  ") call abort()
   if (namea(9).ne."        ") call abort()
   if (namea(15).ne."        ") call abort()
   if (nameb(1).ne."spi01h  ") call abort()
   if (nameb(6).ne."        ") call abort()
   if (nameb(15).ne."        ") call abort()
   
   close (12)
end program pr24794
