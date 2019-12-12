c { dg-do run }
c { dg-skip-if "non-standard options" { *-*-* } { "-ffixed-line-length*" } }
c { dg-options "-fno-pad-source" }
      character(80) a
      a = 'abc
     +def'
      if (a .ne. 'abcdef') stop 1
      end
