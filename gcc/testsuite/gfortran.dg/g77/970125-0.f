c { dg-do compile }
c
c { dg-additional-options "-w" }
c
C JCB comments:
C g77 doesn't accept the added line "integer(kind=7) ..." --
C it crashes!
C 
C It's questionable that g77 DTRT with regarding to passing
C %LOC() as an argument (thus by reference) and the new global
C analysis.  I need to look into that further; my feeling is that
C passing %LOC() as an argument should be treated like passing an
C INTEGER(KIND=7) by reference, and no more specially than that
C (and that INTEGER(KIND=7) should be permitted as equivalent to
C INTEGER(KIND=1), INTEGER(KIND=2), or whatever, depending on the
C system's pointer size).
C 
C The back end *still* has a bug here, which should be fixed,
C because, currently, what g77 is passing to it is, IMO, correct.

C No options:
C ../../egcs/gcc/f/info.c:259: failed assertion `ffeinfo_types_[basictype][kindtype] != NULL'
C -fno-globals -O:
C ../../egcs/gcc/expr.c:7291: Internal compiler error in function expand_expr

c     Frontend bug fixed by JCB 1998-06-01 com.c &c changes.

        integer i4
        integer(kind=8) i8
        integer(kind=8) max4
        data max4/2147483647/
        i4 = %loc(i4)
        i8 = %loc(i8)
        print *, max4
        print *, i4, %loc(i4)
        print *, i8, %loc(i8)
        call foo(i4, %loc(i4), i8, %loc(i8))  ! { dg-error "Type mismatch in argument 'i8a' at .1.; passed INTEGER.8. to INTEGER.4." }
        end
        subroutine foo(i4, i4a, i8, i8a)
        integer(kind=7) i4a, i8a  ! { dg-error "Kind 7 not supported for type INTEGER" }
        integer(kind=8) i8
        print *, i4, i4a
        print *, i8, i8a
        end
