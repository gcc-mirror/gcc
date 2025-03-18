*> { dg-do run }
*> { dg-output {123(\n|\r\n|\r)} }
*> { dg-output {16146(\n|\r\n|\r)} }
*> { dg-output {0x0000000000003f12(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {123(\n|\r\n|\r)} }
*> { dg-output {16146(\n|\r\n|\r)} }
*> { dg-output {0x0000000000003f12(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {\+123(\n|\r\n|\r)} }
*> { dg-output {15378(\n|\r\n|\r)} }
*> { dg-output {0x0000000000003c12(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {\-123(\n|\r\n|\r)} }
*> { dg-output {15634(\n|\r\n|\r)} }
*> { dg-output {0x0000000000003d12(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {properly FALSE(\n|\r\n|\r)} }
*> { dg-output {properly TRUE(\n|\r\n|\r)} }
*> { dg-output {properly FALSE} }
        identification division.
        program-id. packed.
        data division.
        working-storage section.
        01 filler.
            02 as-num binary-double unsigned.
            02 as-hex redefines as-num pointer.
        01 filler.
          02 p1  pic 999 comp-3 value 1.
          02 dp1 redefines p1 binary-short unsigned.
        01 filler.
          02 sp1  pic s999 comp-3 value 1.
          02 sdp1 redefines sp1 binary-short unsigned.
        procedure division.
        move 123 to p1
        display p1
        display dp1
        move dp1 to as-num.
        display as-hex.
        display space
        move -123 to p1
        display p1
        display dp1
        move dp1 to as-num.
        display as-hex.
        display space
        move 123 to sp1
        display sp1
        display sdp1
        move sdp1 to as-num.
        display as-hex.
        display space
        move -123 to sp1
        display sp1
        display sdp1
        move sdp1 to as-num.
        display as-hex.
        display space
        move 2 to p1
        move 2 to sp1
        if p1 < sp1
            DISPLAY "improperly TRUE"
        else
            DISPLAY "properly FALSE".
        if p1 = sp1
            DISPLAY "properly TRUE"
        else
            DISPLAY "improperly FALSE".
        if p1 > sp1
            DISPLAY "improperly TRUE"
        else
            DISPLAY "properly FALSE".
        stop run.
