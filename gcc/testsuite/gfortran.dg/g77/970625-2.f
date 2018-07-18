* Date: Wed, 25 Jun 1997 12:48:26 +0200 (MET DST)
* MIME-Version: 1.0
* From: R.Hooft@EuroMail.com (Rob Hooft)
* To: g77-alpha@gnu.ai.mit.edu
* Subject: Re: testing 970624.
* In-Reply-To: <199706251027.GAA07892@churchy.gnu.ai.mit.edu>
* References: <199706251018.MAA21538@nu>
* <199706251027.GAA07892@churchy.gnu.ai.mit.edu>
* X-Mailer: VM 6.30 under Emacs 19.34.1
* Content-Type: text/plain; charset=US-ASCII
* 
* >>>>> "CB" == Craig Burley <burley@gnu.ai.mit.edu> writes:
* 
*  CB> but OTOH I'd like to see more problems like this on other
*  CB> applications, and especially other systems
* 
* How about this one: An application that prints "112." on all
* compilers/platforms I have tested, except with the new g77 on ALPHA (I
* don't have the new g77 on any other platform here to test)?
* 
* Application Appended. Source code courtesy of my boss.....
* Disclaimer: I do not know the right answer, or even whether there is a
* single right answer.....
* 
* Regards,
* -- 
* ===== R.Hooft@EuroMail.com   http://www.Sander.EMBL-Heidelberg.DE/rob/ ==
* ==== In need of protein modeling?  http://www.Sander.EMBL-Heidelberg.DE/whatif/
* Validation of protein structures?  http://biotech.EMBL-Heidelberg.DE:8400/ ====
* == PGPid 0xFA19277D == Use Linux!  Free Software Rules The World! =============
* 
* nu[152]for% cat humor.f      
      PROGRAM SUBROUTINE
      LOGICAL ELSE IF
      INTEGER REAL, GO TO PROGRAM, WHILE, THEN, END DO
      REAL FORMAT(2)
      DATA IF,REAL,END DO,WHILE,FORMAT(2),I2/2,6,7,1,112.,1/
      DO THEN=1, END DO, WHILE
         CALL = END DO - IF
         PROGRAM = THEN - IF
         ELSE IF = THEN .GT. IF
         IF (THEN.GT.REAL) THEN
            CALL FUNCTION PROGRAM (ELSE IF, GO TO PROGRAM, THEN) ! { dg-error "Type mismatch in argument" }
         ELSE IF (ELSE IF) THEN
            REAL = THEN + END DO
         END IF
      END DO
 10   FORMAT(I2/I2) = WHILE*REAL*THEN
      IF (FORMAT(I2) .NE. FORMAT(I2+I2)) STOP 1
      END ! DO
      SUBROUTINE FUNCTION PROGRAM (REAL,INTEGER, LOGICAL)
      LOGICAL REAL
      REAL LOGICAL
      INTEGER INTEGER, STOP, RETURN, GO TO
      ASSIGN 9 TO STOP     ! { dg-warning "ASSIGN" }
      ASSIGN = 9 + LOGICAL
      ASSIGN 7 TO RETURN   ! { dg-warning "ASSIGN" }
      ASSIGN 9 TO GO TO    ! { dg-warning "ASSIGN" }
      GO TO = 5
      STOP = 8
      IF (.NOT.REAL) GOTO STOP ! { dg-warning "Assigned GOTO" }
      IF (LOGICAL.GT.INTEGER) THEN
         IF = LOGICAL +5
         IF (LOGICAL.EQ.5) ASSIGN 5 TO IF ! { dg-warning "ASSIGN" }
         INTEGER=IF
      ELSE
         IF (ASSIGN.GT.STOP) ASSIGN 9 TO GOTO ! { dg-warning "ASSIGN" }
         ELSE = GO TO
         END IF = ELSE + GO TO
         IF (.NOT.REAL.AND.GOTO.GT.ELSE) GOTO RETURN ! { dg-warning "Assigned GOTO" }
      END IF
    5 CONTINUE
    7 LOGICAL=LOGICAL+STOP
    9 RETURN
      END ! IF
* nu[153]for% f77 humor.f
* nu[154]for% ./a.out
*    112.0000    
* nu[155]for% f90 humor.f  
* nu[156]for% ./a.out    
*    112.0000    
* nu[157]for% g77 humor.f 
* nu[158]for% ./a.out    
*   40.
