      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_CONVERT.out" }
        identification      division.
        program-id.         conv.
        environment         division.
        configuration       section.
        special-names.
            locale sbc  is "cp1252"
            locale ebcd is "ibm1140".
        object-computer.
            gnu-linux
                classification
                    for alphanumeric is sbc
                    for national is ebcd.
        data                division.
        working-storage     section.
        01 hello-a pic X(12) value  "I am ascii".
        01 hello-e pic N(12) value N"I am ebcdic".
        01 hex-a   pic X(4)   value  "01F9".
        01 hex-e   pic N(4)   value N"F109".
        procedure           division.
        display hello-a space function hex-of(hello-a)
        display hello-e space function hex-of(hello-e)
        display hex-a space function hex-of(hex-a)
        display hex-e space function hex-of(hex-e)

        display function convert(hello-a ANY ANUM HEX)
        display function convert(hello-a ANY NAT HEX)

        display function convert(hello-e ANY ANUM HEX)
        display function convert(hello-e ANY NAT HEX)

        display function convert(hex-a HEX BYTE)
        display function convert(hex-e HEX BYTE)

        display function convert(hello-a ANUM NAT) 
              space FUNCTION HEX-OF (function convert(hello-a ANUM NAT))
        display function convert(hello-e NAT ANUM)
              space FUNCTION HEX-OF (function convert(hello-e NAT ANUM))

        goback.
        end program         conv.


