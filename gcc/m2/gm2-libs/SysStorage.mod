(* SysStorage.mod provides dynamic allocation for the system components.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SysStorage ;

FROM libc IMPORT malloc, free, realloc, memset, getenv, printf ;
FROM Debug IMPORT Halt ;
FROM SYSTEM IMPORT ADR ;


CONST
   enableDeallocation =  TRUE ;
   enableZero         =  TRUE ;
   enableTrace        =  FALSE ;

VAR
   callno: CARDINAL ;
   zero,
   trace : BOOLEAN ;


PROCEDURE ALLOCATE (VAR a: ADDRESS ; size: CARDINAL) ;
BEGIN
   a := malloc (size) ;
   IF a = NIL
   THEN
      Halt ('out of memory error',
            __FILE__, __FUNCTION__, __LINE__)
   END ;
   IF enableTrace AND trace
   THEN
      printf ("<DEBUG-CALL> %d SysStorage.ALLOCATE (0x%x, %d bytes)\n", callno, a, size) ;
      printf ("<MEM-ALLOC> %ld %d\n", a, size);
      INC (callno)
   END
END ALLOCATE ;


PROCEDURE DEALLOCATE (VAR a: ADDRESS; size: CARDINAL);
BEGIN
   IF enableTrace AND trace
   THEN
      printf ("<DEBUG-CALL> %d SysStorage.DEALLOCATE (0x%x, %d bytes)\n", callno, a, size) ;
      INC (callno)
   END ;
   IF enableZero AND zero
   THEN
      IF enableTrace AND trace
      THEN
         printf ("  memset (0x%x, 0, %d bytes)\n", a, size)
      END ;
      IF memset (a, 0, size) # a
      THEN
         Halt ('memset should have returned the first parameter',
               __FILE__, __FUNCTION__, __LINE__)
      END
   END ;
   IF enableDeallocation
   THEN
      IF enableTrace AND trace
      THEN
         printf ("  free (0x%x)   %d bytes\n", a, size) ;
         printf ("<MEM-FREE> %ld %d\n", a, size);
      END ;
      free (a)
   END ;
   a := NIL
END DEALLOCATE ;


(*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*)

PROCEDURE REALLOCATE (VAR a: ADDRESS; size: CARDINAL) ;
BEGIN
   IF a = NIL
   THEN
      ALLOCATE (a, size)
   ELSE
      IF enableTrace AND trace
      THEN
         printf ("<DEBUG-CALL> %d SysStorage.REALLOCATE (0x%x, %d bytes)\n", callno, a, size) ;
         INC (callno)
      END ;
      IF enableTrace AND trace
      THEN
         printf ("  realloc (0x%x, %d bytes)  ->  ", a, size) ;
         printf ("<MEM-FREE> %ld %d\n", a, size)
      END ;
      a := realloc (a, size) ;
      IF a = NIL
      THEN
         Halt ('out of memory error',
               __FILE__, __FUNCTION__, __LINE__)
      END ;
      IF enableTrace AND trace
      THEN
         printf ("<MEM-ALLOC> %ld %d\n", a, size) ;
         printf ("  0x%x  %d bytes\n", a, size)
      END
   END
END REALLOCATE ;


PROCEDURE Available (size: CARDINAL) : BOOLEAN;
VAR
   a: ADDRESS ;
BEGIN
   IF enableTrace AND trace
   THEN
      printf ("<DEBUG-CALL> %d SysStorage.Available (%d bytes)\n", callno, size) ;
      INC (callno)
   END ;
   a := malloc (size) ;
   IF a = NIL
   THEN
      IF enableTrace AND trace
      THEN
         printf ("   no\n", size)
      END ;
      RETURN FALSE
   ELSE
      IF enableTrace AND trace
      THEN
         printf ("   yes\n", size)
      END ;
      free (a) ;
      RETURN TRUE
   END
END Available ;


(*
   Init - initializes the heap.  This does nothing on a GNU/Linux system.
          But it remains here since it might be used in an embedded system.
*)

PROCEDURE Init ;
END Init ;


BEGIN
   callno := 0 ;
   IF enableTrace
   THEN
      trace := getenv (ADR ("M2DEBUG_SYSSTORAGE_trace")) # NIL
   ELSE
      trace := FALSE
   END ;
   IF enableZero
   THEN
      zero := getenv (ADR ("M2DEBUG_SYSSTORAGE_zero")) # NIL
   ELSE
      zero := FALSE
   END
END SysStorage.
