c { dg-do run }
      character*120 file
      character*5   string
      file = "c:/dos/adir/bdir/cdir/text.doc"
      write(string, *) "a ", file
      if (string .ne. ' a') call abort
C-- The leading space is normal for list-directed output
C-- "file" is not printed because it would overflow "string".
      end
