c { dg-do compile }
c { dg-options "-O2 -std=f95" }
       LOGICAL(kind=1) l1
       LOGICAL(kind=2) l2
       LOGICAL         l4
       INTEGER(kind=1) i1
       INTEGER(kind=2) i2
       INTEGER         i4

       i1 = .TRUE.  ! { dg-error "convert" }
       i2 = .TRUE.  ! { dg-error "convert" }
       i4 = .TRUE.  ! { dg-error "convert" }

       i1 = .FALSE. ! { dg-error "convert" }
       i2 = .FALSE. ! { dg-error "convert" }
       i4 = .FALSE. ! { dg-error "convert" }

       i1 = l1      ! { dg-error "convert" }
       i2 = l1      ! { dg-error "convert" }
       i4 = l1      ! { dg-error "convert" }

       i1 = l2      ! { dg-error "convert" }
       i2 = l2      ! { dg-error "convert" }
       i4 = l2      ! { dg-error "convert" }

       i1 = l4      ! { dg-error "convert" }
       i2 = l4      ! { dg-error "convert" }
       i4 = l4      ! { dg-error "convert" }

       l1 = i1      ! { dg-error "convert" }
       l2 = i1      ! { dg-error "convert" }
       l4 = i1      ! { dg-error "convert" }

       l1 = i2      ! { dg-error "convert" }
       l2 = i2      ! { dg-error "convert" }
       l4 = i2      ! { dg-error "convert" }

       l1 = i4      ! { dg-error "convert" }
       l2 = i4      ! { dg-error "convert" }
       l4 = i4      ! { dg-error "convert" }

       END
 
