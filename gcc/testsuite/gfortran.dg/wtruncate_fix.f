c { dg-do compile }
c { dg-options "-Wall" }
c PR42852 -Wall warns about truncated lines when only a continuation character is truncated
      print *, "Hello!"                                                  &   !xxxxx
     & //   " World!"
      print *, "Hello!"                                                  &   xxxxx
     & //   " World!"
      print *, "Hello!"                                                  //
     & //   " World!"
      end
c { dg-warning "Line truncated" " " { target *-*-* } 6 }
c { dg-warning "Line truncated" " " { target *-*-* } 8 }
