       *> { dg-do run }
       *> { dg-output-file "group2/Quote_marks_in_comment_paragraphs.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    prog.
       DATE-written.  hello'".
      *> Written is intentionally lowercase.
      *> extra " to fix syntax highlighting
       PROCEDURE      DIVISION.
           DISPLAY "Hello, world!".

