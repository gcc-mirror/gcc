c { dg-do compile }
c { dg-options "-std=legacy" }
c
CCC Abort fixed by:
CCC1998-04-21  Jim Wilson  <wilson@cygnus.com>
CCC
CCC        * stmt.c (check_seenlabel): When search for line number note for
CCC        warning, handle case where there is no such note.
      logical l(10)
      integer i(10)
      goto (10,20),l ! { dg-error "Selection expression in computed GOTO" }
      goto (10,20),i ! { dg-error "Selection expression in computed GOTO" }
 10   stop
 20   end
