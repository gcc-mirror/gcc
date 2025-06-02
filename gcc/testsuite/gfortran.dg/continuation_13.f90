! { dg-do run }
! { dg-options "-std=legacy" }
! PR64506
character(25) :: astring

100 format('This format is OK.'&
    )
200 format('This format now works.'&!comment
    )
300 format('This format now works.'& !comment
    )
400 format('This format is OK.' &!comment
    )
500 format('This format is OK.' & !comment
    )
600 format('This format now works.'''&!comment
    )
700 format('This format now works.'''& !comment
    )
! See PR65903 for the following cases.
800 format('This is actually ok.'& !comment
   ' end'  )
900 format('This is actually ok.' & !comment
   ' end'  )
write(astring,100)
if (astring.ne."This format is OK.") STOP 1
write(astring,200)
if (astring.ne."This format now works.") STOP 2
write(astring,300)
if (astring.ne."This format now works.") STOP 3
write(astring,400)
if (astring.ne."This format is OK.") STOP 4
write(astring,500)
if (astring.ne."This format is OK.") STOP 5
write(astring,600)
if (astring.ne."This format now works.'") STOP 6
write(astring,700)
if (astring.ne."This format now works.'") STOP 7
write(astring,800)
if (astring.ne."This is actually ok.' end") STOP 8
write(astring,900)
if (astring.ne."This is actually ok. end") STOP 9

end
