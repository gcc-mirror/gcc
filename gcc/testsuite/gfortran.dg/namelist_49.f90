! { dg-do run }
! { dg-options "-fbackslash" }
! PR36546 Namelist error with tab following a comma and newline
      program check1
      real a,b,c
      namelist/CASEDAT/A,B,C
      open(1, status="scratch")
      write(1,'(a)') "&CASEDAT"
      write(1,'(a)') "\t\tA = 1.0,\t\tB = 2.0,"
      write(1,'(a)') "\t\tC = 3.0,"
      write(1,'(a)') " /"
      rewind(1)
      a = 0.0
      b = 0.0
      c = 0.0
      read(1,casedat)
      if ((a.ne.1.0) .or. (b.ne.2.0) .or. (c.ne.3.0)) call abort
      end

