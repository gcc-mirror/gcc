! { dg-do run }
! { dg-options "-std=gnu" }
! PR64506 fixed form source
      character(25) :: astring

 100  format('This format is OK.'
     &)
 200  format('This format works now.'!comment << FAILS
     &)
 300  format('This format is OK.' !comment
     &)
 400  format('This format is OK.'  !comment
     &)
 500  format('This format is now OK.'''!comment
     &   )
 600  format('This format is OK.''' !comment
     &   )
      write(astring,100)
      if (astring.ne."This format is OK.") STOP 1
      write(astring,200)
      if (astring.ne."This format works now.") STOP 2
      write(astring,300)
      if (astring.ne."This format is OK.") STOP 3
      write(astring,400)
      if (astring.ne."This format is OK.") STOP 4
      write(astring,500)
      if (astring.ne."This format is now OK.'") STOP 5
      write(astring,600)
      if (astring.ne."This format is OK.'") STOP 6
      end
