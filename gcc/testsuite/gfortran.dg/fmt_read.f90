! { dg-do run }
! pr18398, missing data on sequential formatted reads
! test contributed by Thomas.Koenig@online.de
      open(7,status='scratch')
      write (7,'(F12.5)') 1.0, 2.0, 3.0
      rewind(7)
      read(7,'(F15.5)') a,b
!   note the read format is wider than the write
      if (abs(a-1.0) .gt. 1e-5) call abort
      if (abs(b-2.0) .gt. 1e-5) call abort
      end
