C Causes internal compiler error on egcs 1.0.1 on i586-pc-sco3.2v5.0.4
C To: egcs-bugs@cygnus.com
C Subject: backend case range problem/fix
C From: Dave Love <d.love@dl.ac.uk>
C Date: 02 Dec 1997 18:11:35 +0000
C Message-ID: <rzqpvnfboo8.fsf@djlvig.dl.ac.uk>
C 
C The following Fortran test case aborts the compiler because
C tree_int_cst_lt dereferences a null tree; this is a regression from
C gcc 2.7.
C 
C The patch is against egcs sources.  I don't know if it's still
C relevant to mainline gcc, which I no longer follow.

      INTEGER N
      READ(*,*) N
      SELECT CASE (N)
        CASE (1:)
           WRITE(*,*) 'case 1'
        CASE (0)
           WRITE(*,*) 'case 0'
      END SELECT
      END

