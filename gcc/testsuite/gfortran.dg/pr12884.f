c { dg-do run }
c pr 12884
c test namelist with input file containg / before namelist. Also checks
c non-standard use of $ instead of &
c Based on example provided by jean-pierre.flament@univ-lille1.fr

      program pr12884
      integer ispher,nosym,runflg,noprop
      namelist /cntrl/ ispher,nosym,runflg,noprop
      ispher = 0
      nosym = 0
      runflg = 0
      noprop = 0 
      open (10, status = "scratch")
      write (10, '(A)') " $FILE"
      write (10, '(A)') "   pseu  dir/file"
      write (10, '(A)') " $END"
      write (10, '(A)') " $cntrl ispher=1,nosym=2,"
      write (10, '(A)') " runflg=3,noprop=4,$END"
      write (10, '(A)')"/"
      rewind (10)
      read (10, cntrl)
      if ((ispher.ne.1).or.(nosym.ne.2).or.(runflg.ne.3).or.
     &  (noprop.ne.4)) call abort ()
      end
