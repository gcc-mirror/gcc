c { dg-do run }
c { dg-skip-if "non-standard options" { *-*-* } { "-ffixed-line-length*" } }
c { dg-options "-fpad-source" }
      character(80) a
      a = 'abc
     +def'
      if (a(:61) .ne. 'abc') stop 1
      if (a(62:) .ne. 'def') stop 2
      end
