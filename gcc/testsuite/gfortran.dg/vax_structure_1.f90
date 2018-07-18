! { dg-do compile }
! { dg-options "-fdec-structure" }
! Tests the VAX STRUCTURE and RECORD statements.
! These are syntactic sugar for TYPE statements.

      program vax_structure_1
      structure /stocklevel/
         integer*2   A
         integer*4   B
         integer*4   CS(0:15)
         byte        D(0:15)
      end structure

      record /stocklevel/ rec1, recs(100)
      integer x
      integer*2 y

      rec1.A = 100
      recs(100).CS(10)=1
      x = 150
      y = 150

      print *, rec1.B.eq.100
      print *, rec1.A.eq.x ! {dg-error "are INTEGER(2)/INTEGER(4)"}
      print *, rec1.A.eq.y
      print *, recs(100).CS(10)
      end program
