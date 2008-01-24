C { dg-do compile }
C Tests the fix for the regression PR34872, in which the re-matching of
C the function declaration made a mess if the first executable statement
C had a label.
      CHARACTER FUNCTION s()
   10 CONTINUE
      GOTO 10
      s = ' '
      END FUNCTION s

      CHARACTER FUNCTION t()
   10 format ("q")
      write (t, 10)
      END FUNCTION t

      character t
      if (t() .ne. "q") call abort ()
      end
