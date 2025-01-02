IMPLEMENTATION MODULE opaquevariant ;  (*!m2pim*)

TYPE
   colour = (red, blue, green) ;
   Opaque = POINTER TO RECORD
                          CASE key: colour OF

                          red  :  truefield : TrueRec |
                          blue,
                          green:  falsefield: FalseRec

                          END
                       END ;

   TrueRec = RECORD
                a: Opaque
             END ;

   FalseRec = RECORD
                 b: Opaque
              END ;

VAR
   v: Opaque ;

END opaquevariant.
