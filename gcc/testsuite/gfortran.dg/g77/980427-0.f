c { dg-do compile }
c ../../egcs/gcc/f/com.c:938: failed assertion `TREE_CODE (TREE_TYPE (e)) == REAL_TYPE'
c     Fixed by 28-04-1998 global.c (ffeglobal_ref_progunit_) change.
      external b
      call y(b)
      end
      subroutine x
      a = b()
      end
