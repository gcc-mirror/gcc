! { dg-do run }
! PR46705 Spurious "Missing '&' in continued character constant" warning occurs twice
character(15) :: astring
1 FORMAT (''&
   ' abcdefg x')
write(astring, 1)
if (astring.ne."' abcdefg x") call abort
END
