c { dg-do compile }
c { dg-options "-O2 -std=legacy" }
       LOGICAL(kind=1) l1
       LOGICAL(kind=2) l2
       LOGICAL         l4
       INTEGER(kind=1) i1
       INTEGER(kind=2) i2
       INTEGER         i4

       i1 = .TRUE.
       i2 = .TRUE.
       i4 = .TRUE.

       i1 = .FALSE.
       i2 = .FALSE.
       i4 = .FALSE.

       i1 = l1
       i2 = l1
       i4 = l1

       i1 = l2
       i2 = l2
       i4 = l2

       i1 = l4
       i2 = l4
       i4 = l4

       l1 = i1
       l2 = i1
       l4 = i1

       l1 = i2
       l2 = i2
       l4 = i2

       l1 = i4
       l2 = i4
       l4 = i4

       END
 
