! { dg-do run }
! PR121234 Bogus diagnostic on READ of string with semicolon.
  character(12) buffer,a
  a = 'xxxxxxxxxx'
  buffer="33;44"
  read(buffer,*) a
  if (a .ne. "33;44") stop 1
  a = 'xxxxxxxxxx'
  buffer="  ;;33 ,44 "
  read(buffer,*,decimal="comma") a
  if (a .ne. 'xxxxxxxxxx') stop 2 ! A null read
  a = 'xxxxxxxxxx'
  buffer="  ;;33 ,44 "
  read(buffer,*,decimal="point") a
  if (a .ne. ';;33') stop 3 ! Spaces are delimiting
  a = 'xxxxxxxxxx'
  buffer=";;33;,44 "
  read(buffer,*) a
  if (a .ne. ';;33;') stop 4 ! Comma is delimiting
  a = 'xxxxxxxxxx'
  buffer=";;33;44;; "
  read(buffer,*) a
  if (a .ne. ';;33;44;;') stop 5 ! Space is delimiting
  a = 'xxxxxxxxxx'
  buffer=";;33;44;;;.7"
  read(buffer,*) a
  if (a .ne. ';;33;44;;;.7') stop 6 ! Space is delimiting
end
