c { dg-do run }
      character*5   string
      write(string, *) "a "
      if (string .ne. ' a') call abort
C-- The leading space is normal for list-directed output

      end
