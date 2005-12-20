! { dg-do run }
! PR25305 Check for no end-of-record error on write after read.
! Derived from example given in PR by Grigory Zagorodnev.
! Committed by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
       CHARACTER RD*8
       CHARACTER WR*8
       CHARACTER(1) C1
       RD='N 1'
       READ (RD(1:2),*) C1
       IF (C1.NE.'N') CALL ABORT
       WRITE (WR,*) 'passed'
       IF (WR.NE.' passed') CALL ABORT()
       END

