* egcs-bugs:
* From: Martin Kahlert <martin.kahlert@mchp.siemens.de>
* Subject: ICE in g77 from egcs-19981109
* Message-Id: <199811101134.MAA29838@keksy.mchp.siemens.de>

* As of 1998-11-17, fails -O2 -fomit-frame-pointer with 
* egcs/gcc/testsuite/g77.f-torture/compile/981117-1.f:8: internal error--insn does not satisfy its constraints:
* (insn 31 83 32 (set (reg:SF 8 %st(0))
*         (mult:SF (reg:SF 8 %st(0))
*             (const_double:SF (mem/u:SF (symbol_ref/u:SI ("*.LC1")) 0) 0 0 1073643520))) 350 {strlensi-3} (nil)
*     (nil))
* ../../egcs/gcc/toplev.c:1390: Internal compiler error in function fatal_insn

* Fixed sometime before 1998-11-21 -- don't know by which change.

      SUBROUTINE SSPTRD
      PARAMETER (HALF = 0.5 )
      DO I = 1, N
         CALL SSPMV(TAUI)
         ALPHA = -HALF*TAUI
         CALL SAXPY(ALPHA)
      ENDDO
      END
