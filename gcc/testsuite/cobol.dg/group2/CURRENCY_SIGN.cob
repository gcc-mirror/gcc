       *> { dg-do run }
       *> { dg-output-file "group2/CURRENCY_SIGN.out" }

       PROGRAM-ID.   prog.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Y".

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  amount    pic Y(6)9.99.

       PROCEDURE DIVISION.
           Move 1512.34 to Amount
           Display "Amount is #" Amount '#' with no advancing.

           GOBACK
           .
       END PROGRAM prog.

