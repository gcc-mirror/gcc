! { dg-do run }
! pr19314 inquire(..position=..) segfaults
! test by Thomas.Koenig@online.de
!         bdavis9659@comcast.net
      implicit none
      character*20 chr
      open(7,STATUS='SCRATCH')
      inquire(7,position=chr)
      if (chr.NE.'ASIS') CALL ABORT
      close(7)
      open(7,STATUS='SCRATCH',ACCESS='DIRECT',RECL=100)
      inquire(7,position=chr)
      if (chr.NE.'UNDEFINED') CALL ABORT
      close(7)
      open(7,STATUS='SCRATCH',POSITION='REWIND')
      inquire(7,position=chr)
      if (chr.NE.'REWIND') CALL ABORT
      close(7)
      open(7,STATUS='SCRATCH',POSITION='ASIS')
      inquire(7,position=chr)
      if (chr.NE.'ASIS') CALL ABORT
      close(7)
      open(7,STATUS='SCRATCH',POSITION='APPEND')
      inquire(7,position=chr)
      if (chr.NE.'APPEND') CALL ABORT
      close(7)
      open(7,STATUS='SCRATCH',POSITION='REWIND')
      write(7,*)'this is a record written to the file'
      inquire(7,position=chr)
      if (chr.NE.'ASIS') CALL ABORT
      rewind(7)
      inquire(7,position=chr)
      if (chr.NE.'REWIND') CALL ABORT
      close(7)
      end
