* Date: Wed, 13 Aug 1997 15:34:23 +0200 (METDST)
* From: Claus Denk <denk@cica.es>
* To: g77-alpha@gnu.ai.mit.edu
* Subject: 970811 report - segfault bug on alpha still there
*[...]
* Now, the bug that I reported some weeks ago is still there, I'll post
* the test program again:
*
        PROGRAM TEST
C       a bug in g77-0.5.21 - alpha. Works with NSTART=0 and segfaults with
C       NSTART=1 on the second write.
        PARAMETER (NSTART=1,NADD=NSTART+1)
        REAL AB(NSTART:NSTART)
        AB(NSTART)=1.0
        I=1
        J=2
        IND=I-J+NADD
        write(*,*) AB(IND)
        write(*,*) AB(I-J+NADD)
        END
