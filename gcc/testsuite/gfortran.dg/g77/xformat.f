c { dg-do compile }
       PRINT 10, 2, 3
10     FORMAT (I1, X, I1) ! { dg-warning "Extension: X descriptor" "Extension: X descriptor"  }
       END
