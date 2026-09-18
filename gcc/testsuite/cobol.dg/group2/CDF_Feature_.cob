      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-fexec-charset=ibm1140 -dialect ibm" }
       *> { dg-output-file "group2/CDF_Feature_.out" }

       id division.
       program-id. prog.
       Data Division.
       Working-Storage Section.
         77 X PIC 9 value 1.
       procedure division.
           >>IF %64-BIT-POINTER DEFINED
           DISPLAY '64-bit-pointer mode ON'
           >>END-IF
           >>IF %EBCDIC-MODE DEFINED
           DISPLAY 'EBCDIC-MODE ON'
           >>END-IF
           >>DEFINE %64-BIT-POINTER OFF
           >>IF %64-BIT-POINTER DEFINED
           DISPLAY '64-bit-pointer mode still ON'
           >>END-IF
           >>IF not-ok IS DEFINED
             >>DEFINE %EBCDIC-MODE OFF
             >>IF %EBCDIC-MODE DEFINED
               DISPLAY 'EBCDIC-MODE mode still ON'
             >>END-IF
           >>END-IF

