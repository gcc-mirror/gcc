      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/Simple_COMP-X.out" }
        identification          division.
        program-id.             compx.
        data                    division.
        working-storage         section.
        01 byte.
          02 byte-val           pic x(1) comp-x.
        01 short.               
          02 short-val          pic x(2) comp-x.
        01 long.                
          02 long-val           pic x(4) comp-x.
        01 longlong.            
          02 longlong-val       pic x(8) comp-x.
        01 sixteenbytes.        
          02 sixteenbytes-val   pic x(16) comp-x.
        procedure               division.
            move high-values to byte short long longlong sixteenbytes
            display function hex-of(byte)
            display function hex-of(short)
            display function hex-of(long)
            display function hex-of(longlong)
            display function hex-of(sixteenbytes)
            display "byte-val is: " byte-val.
            display "short-val is: " short-val.
            display "long-val is: " long-val.
            display "longlong-val is: " longlong-val.
            display "sixteenbytes-val is messed up and needs fixing"
            goback.
        end  program            compx.

