C { dg-do compile }
C PR 18993 
C we didn't match the end of statement following NULLIFY ()
C this lead to weird error messages
      subroutine ordern( ) 
      real, pointer :: aux(:,:) 
C Nullify pointers 
      nullify(aux)
C Set default sizes for order N arrays
      end subroutine ordern

