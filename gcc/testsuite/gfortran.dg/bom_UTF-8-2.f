#define a(i) b(i)
!
! PR fortran/128343 - UTF-8 byte order mark and internal line buffer
!
! { dg-do compile }
! { dg-additional-options "-w" }
!
! Note: this file has the following characters at the beginning:
!
! 0000000 357 273 277 043 144 145 146 151 156 145 040 141 050 151 051 040
!         357 273 277   #   d   e   f   i   n   e       a   (   i   )    
!
! which lead to invalid write/read errors under valgrind.
!
! Derived from 1066_0007.f from the Fujitsu test suite on Github.

#if 0
#else ! a
      ! a
      write(10,*) a
      print *,'pass'
#endif ! a a
      end

﻿! <- here's an unsolicited BOM { dg-error "Invalid character 0xBB" }

! { dg-prune-output "Non-numeric character in statement label" }
