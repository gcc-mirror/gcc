*> { dg-do run }
*> { dg-output {0x0000000000000000 Should be 0x0000000000000000(\n|\r\n|\r)} }
*> { dg-output {0x0000000020202020 Should be 0x0000000020202020(\n|\r\n|\r)} }
*> { dg-output {0x0000000030303030 Should be 0x0000000030303030(\n|\r\n|\r)} }
*> { dg-output {0x0000000022222222 Should be 0x0000000022222222(\n|\r\n|\r)} }
*> { dg-output {0x00000000ffffffff Should be 0x00000000ffffffff} }
      *> This program is a sanity check of COMP-5 moves and addition.
        program-id. comp5.
        data division.
        working-storage section.
        77 var PIC 999V999 COMP-5 .
        77 var1 PIC 999V9(1) COMP-5 .
        77 var2 PIC 999V9(2) COMP-5 .
        77 var3 PIC 999V9(3) COMP-5 .
        77 var4 PIC 999V9(4) COMP-5 .
        77 var5 PIC 999V9(5) COMP-5 .
        77 var6 PIC 999V9(6) COMP-5 .
        77 var7 PIC 999V9(7) COMP-5 .
        77 var8 PIC 999V9(8) COMP-5 .
        77 var555 PIC 999V99999999 COMP-5 VALUE 555.55555555.
        01 C-5A PIC X(4)  VALUE LOW-VALUE.
        01 C-5B PIC X(4)  VALUE SPACE.
        01 C-5C PIC X(4)  VALUE ZERO.
        01 C-5D PIC X(4)  VALUE QUOTE.
        01 C-5E PIC X(4)  VALUE HIGH-VALUE.
        01 PTR POINTER.
        01 PC  REDEFINES PTR PIC X(4).
        procedure division.
        move 111.111 to var.
        if var not equal to 111.111 display var " should be 111.111".
        add 000.001 to var.
        if var not equal to 111.112 display var " should be 111.112".
        add 000.01 to var.
        if var not equal to 111.122 display var " should be 111.122".
        add 000.1 to var.
        if var not equal to 111.222 display var " should be 111.222".
        add 1 to var.
        if var not equal to 112.222 display var " should be 112.222".
        add 10 to var.
        if var not equal to 122.222 display var " should be 122.222".
        add 100 to var.
        if var not equal to 222.222 display var " should be 222.222".
        move 555.55555555 to var1
        move 555.55555555 to var2
        move 555.55555555 to var3
        move 555.55555555 to var4
        move 555.55555555 to var5
        move 555.55555555 to var6
        move 555.55555555 to var7
        move 555.55555555 to var8
        add 0.00000001 TO var555 giving var1 rounded
        add 0.00000001 TO var555 giving var2 rounded
        add 0.00000001 TO var555 giving var3 rounded
        add 0.00000001 TO var555 giving var4 rounded
        add 0.00000001 TO var555 giving var5 rounded
        add 0.00000001 TO var555 giving var6 rounded
        add 0.00000001 TO var555 giving var7 rounded
        add 0.00000001 TO var555 giving var8 rounded
        if var1 not equal to 555.6 display var1 " should be 555.6".
        if var2 not equal to 555.56 display var2 " should be 555.56".
        if var3 not equal to 555.556 display var3 " should be 555.556".
        if var4 not equal to 555.5556 display var4 " should be 555.5556".
        if var5 not equal to 555.55556 display var5 " should be 555.55556".
        if var6 not equal to 555.555556 display var6 " should be 555.555556".
        if var7 not equal to 555.5555556 display var7 " should be 555.5555556".
        if var8 not equal to 555.55555556 display var8 " should be 555.55555556".
        MOVE C-5A TO PC DISPLAY PTR " Should be 0x0000000000000000".
        MOVE C-5B TO PC DISPLAY PTR " Should be 0x0000000020202020".
        MOVE C-5C TO PC DISPLAY PTR " Should be 0x0000000030303030".
        MOVE C-5D TO PC DISPLAY PTR " Should be 0x0000000022222222".
        MOVE C-5E TO PC DISPLAY PTR " Should be 0x00000000ffffffff".
        stop run.
