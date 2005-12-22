c { dg-do compile }
c { dg-options "-O2" }
       LOGICAL(kind=1) l1
       LOGICAL(kind=2) l2
       LOGICAL         l4
       INTEGER(kind=1) i1
       INTEGER(kind=2) i2
       INTEGER         i4

       i1 = .TRUE.  ! { dg-warning "Extension: Conversion" }
       i2 = .TRUE.  ! { dg-warning "Extension: Conversion" }
       i4 = .TRUE.  ! { dg-warning "Extension: Conversion" }

       i1 = .FALSE. ! { dg-warning "Extension: Conversion" }
       i2 = .FALSE. ! { dg-warning "Extension: Conversion" }
       i4 = .FALSE. ! { dg-warning "Extension: Conversion" }

       i1 = l1      ! { dg-warning "Extension: Conversion" }
       i2 = l1      ! { dg-warning "Extension: Conversion" }
       i4 = l1      ! { dg-warning "Extension: Conversion" }

       i1 = l2      ! { dg-warning "Extension: Conversion" }
       i2 = l2      ! { dg-warning "Extension: Conversion" }
       i4 = l2      ! { dg-warning "Extension: Conversion" }

       i1 = l4      ! { dg-warning "Extension: Conversion" }
       i2 = l4      ! { dg-warning "Extension: Conversion" }
       i4 = l4      ! { dg-warning "Extension: Conversion" }

       l1 = i1      ! { dg-warning "Extension: Conversion" }
       l2 = i1      ! { dg-warning "Extension: Conversion" }
       l4 = i1      ! { dg-warning "Extension: Conversion" }

       l1 = i2      ! { dg-warning "Extension: Conversion" }
       l2 = i2      ! { dg-warning "Extension: Conversion" }
       l4 = i2      ! { dg-warning "Extension: Conversion" }

       l1 = i4      ! { dg-warning "Extension: Conversion" }
       l2 = i4      ! { dg-warning "Extension: Conversion" }
       l4 = i4      ! { dg-warning "Extension: Conversion" }

       END
 
