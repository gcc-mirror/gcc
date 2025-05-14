*> { dg-do run }
*> { dg-output {Do a forward\-reference PERFORM para_CCC(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {We are about to fall through the para_AAA, para_BBB, and para_CCC definitions(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_AAA(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {We are about to PERFORM para_AAA(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_AAA(\n|\r\n|\r)} }
*> { dg-output {We are about to PERFORM para_BBB three times(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {We are about to PERFORM para_BBB through para_CCC(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {We are about to PERFORM para_BBB through para_CCC another five times(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_BBB(\n|\r\n|\r)} }
*> { dg-output {   We are inside para_CCC(\n|\r\n|\r)} }
*> { dg-output {Thank you for visiting the PERFORM PARAGRAPH demo} }
IDENTIFICATION DIVISION.
PROGRAM-ID. PerformParagraphs.
PROCEDURE DIVISION.
    DISPLAY "Do a forward-reference PERFORM para_CCC"
    PERFORM para_CCC
    DISPLAY "We are about to fall through the para_AAA, para_BBB, and para_CCC definitions".
para_AAA.
    DISPLAY "   We are inside para_AAA".
para_BBB.
    DISPLAY "   We are inside para_BBB".
para_CCC.
    DISPLAY "   We are inside para_CCC".
para_DDD.
    DISPLAY "We are about to PERFORM para_AAA"
    PERFORM para_AAA
    DISPLAY "We are about to PERFORM para_BBB three times"
    PERFORM para_BBB 3 times
    DISPLAY "We are about to PERFORM para_BBB through para_CCC"
    PERFORM para_BBB through para_CCC
    DISPLAY "We are about to PERFORM para_BBB through para_CCC another five times"
    PERFORM para_BBB through para_CCC 5 times
    DISPLAY "Thank you for visiting the PERFORM PARAGRAPH demo"
    STOP RUN.
    END PROGRAM PerformParagraphs.
