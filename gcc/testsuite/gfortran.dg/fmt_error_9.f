! { dg-do run }
! { dg-options "-std=gnu" }
! PR38439 I/O PD edit descriptor inconsistency
! Test case prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
      character(len=25) :: str
      character(len=132) :: msg, line
      str = '(1pd24.15e6)'
      line = "initial string"
      x = 555.25
      
      write (line,str,iostat=istat, iomsg=msg) 1.0d0, 1.234
      if (istat.ne.5006 .or. msg(1:15).ne."Period required") call abort
      if (line.ne."initial string") call abort

      str = '(1pf0.15)'
      write (line,str,iostat=istat, iomsg=msg) 1.0d0
      if (istat.ne.0) call abort
      read (*,str,iostat=istat, iomsg=msg) x
      if (istat.ne.5006 .or. msg(1:15).ne."Positive width ") call abort
      if (x.ne.555.25) call abort
      
      write (line,'(1pd24.15e11.3)') 1.0d0, 1.234
      if (line.ne."   1.000000000000000D+00  1.234E+00") call abort
      
      str = '(1p2d24.15)'
      msg = "   1.000000000000000D+00   1.233999967575073D+00That's it!"
      write (line,'(1p2d24.15a)') 1.0d0, 1.234, "That's it!"
      if (line.ne.msg) print *, msg
      end
