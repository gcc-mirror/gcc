! { dg-do run { target fd_truncate } }
! pr 19071
! test case provided by
!   Thomas.Koenig@online.de
       program cio      
       complex a      
       real r1,r2      
       a = cmplx(1.0, 2.0)      
       open(unit=74,status='scratch')      
       write(74,'(1P,E13.5)')a      
       rewind(74)
!  can read the complex in as two reals, one on each line
       read(74,'(E13.5)')r1,r2      
       if (r1.ne.1.0 .and. r2.ne.2.0) call abort
       end
