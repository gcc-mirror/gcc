       *> { dg-do run }
       *> { dg-options "-ffixed-form" }
       *> { dg-output-file "group2/Indicators_______________-____D__.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FF2.
      *Asterisk in correct column
      /
       PROCEDURE DIVISION.
           DISPLAY                                                 "gekk
      -"os rule".
           DISPLAY                                                 "gerb
      * ISO says blank and comment lines do not interfere with
      * literal continuation

      -"ils don't rule".
      * "D" is a deprecated feature of COBOL dropped from
      *    the ISO-IEC standard. Lines with "D" in the indicator
      *    column were enabled when OBJECT COMPUTER contained
      *    "WITH DEBUG MODE". Otherwise they were treated as
      *    comments. This behavior is a "vendor extension" to
      *    the current standard but allows old code to be used
      *    as it was prior to the deprecation.
      D    DISPLAY 'Should not display'.
           EXIT PROGRAM.

