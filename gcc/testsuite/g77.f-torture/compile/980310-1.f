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

      INTEGER N
      READ(*,*) N
      SELECT CASE (N)
        CASE (1:)
           WRITE(*,*) 'case 1'
        CASE (0)
           WRITE(*,*) 'case 0'
      END SELECT
      END

C The relevant change to cure this is:
C
C Thu Dec  4 06:34:40 1997  Richard Kenner  <kenner@vlsi1.ultra.nyu.edu>
C
C       * stmt.c (pushcase_range): Clean up handling of "infinite" values.
C

