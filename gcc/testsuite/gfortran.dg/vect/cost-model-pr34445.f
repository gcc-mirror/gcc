c { dg-do compile }
      Subroutine FndSph(Alpha,Rad)
      Dimension Rad(100),RadInp(100)
      Do I = 1, NSphInp
        Rad(I) = RadInp(I)
        Alpha = 1.2
      End Do
      End
c { dg-final { cleanup-tree-dump "vect" } }
