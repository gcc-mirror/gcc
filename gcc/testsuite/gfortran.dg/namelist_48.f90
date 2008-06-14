! { dg-do run }
! { dg-options "-fbackslash" }
! PR36538 namelist failure with tabs preceding object name
      program check1
      integer x
      namelist/casein/x
      open(1, status="scratch")
      write(1,'(a)') "&CASEIN"	
      write(1,'(a)') "\t\tx = 1"
      write(1,'(a)') "/"
      rewind(1)
      x = 0
      read(1,casein)
      if (x.ne.1) call abort
      end
