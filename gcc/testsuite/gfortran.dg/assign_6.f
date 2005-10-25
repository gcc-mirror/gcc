C { dg-do run }
C Option passed to avoid excess errors from obsolete warning
C { dg-options "-w" }
C PR22290

      integer nz
      assign 93 to nz
      go to nz,(93)
  93  continue
      end
