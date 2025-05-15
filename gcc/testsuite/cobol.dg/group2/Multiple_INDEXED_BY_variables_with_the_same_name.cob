       *> { dg-do run }
       *> { dg-output-file "group2/Multiple_INDEXED_BY_variables_with_the_same_name.out" }
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  GROUP-1-TABLE.
           05  TABLE-LEVEL-1 VALUE "ABCDEFGHIJKLMNO".
               06  TABLE-ITEM PICTURE X OCCURS 15 TIMES INDEXED BY IND.
               88  EQUALS-M   VALUE "M".
       01  GROUP-2-TABLE.
           05  TABLE-LEVEL-1 VALUE "abcdefghijklmno".
               06  TABLE-ITEM PICTURE X OCCURS 15 TIMES INDEXED BY IND.
               88  EQUALS-M   VALUE "M".
       PROCEDURE DIVISION.
            set IND OF GROUP-1-TABLE to 2
            set IND OF GROUP-2-TABLE to 4
            display "The output should be ""Db"""
            display "The output        is " """"
                    TABLE-ITEM of GROUP-1-TABLE(IND OF GROUP-2-TABLE)
                    TABLE-ITEM of GROUP-2-TABLE(IND OF GROUP-1-TABLE)
                    """"
            goback.

