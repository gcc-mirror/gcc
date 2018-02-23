! { dg-do run { target fd_truncate } }
!
! pr19314 inquire(..position=..) segfaults
! test by Thomas.Koenig@online.de
!         bdavis9659@comcast.net
      implicit none
      character(len=20) chr
      open(7,STATUS='SCRATCH')
      inquire(7,position=chr)
      if (chr.NE.'ASIS') STOP 1
      close(7)
      open(7,STATUS='SCRATCH',ACCESS='DIRECT',RECL=100)
      inquire(7,position=chr)
      if (chr.NE.'UNDEFINED') STOP 2
      close(7)
      open(7,STATUS='SCRATCH',POSITION='REWIND')
      inquire(7,position=chr)
      if (chr.NE.'REWIND') STOP 3
      close(7)
      open(7,STATUS='SCRATCH',POSITION='ASIS')
      inquire(7,position=chr)
      if (chr.NE.'ASIS') STOP 4
      close(7)
      open(7,STATUS='SCRATCH',POSITION='APPEND')
      inquire(7,position=chr)
      if (chr.NE.'APPEND') STOP 5
      close(7)
      open(7,STATUS='SCRATCH',POSITION='REWIND')
      write(7,*)'this is a record written to the file'
      write(7,*)'this is another record'
      backspace(7)
      inquire(7,position=chr)
      if (chr .NE. 'UNSPECIFIED') STOP 6
      rewind(7)
      inquire(7,position=chr)
      if (chr.NE.'REWIND') STOP 7
      close(7)
      end
