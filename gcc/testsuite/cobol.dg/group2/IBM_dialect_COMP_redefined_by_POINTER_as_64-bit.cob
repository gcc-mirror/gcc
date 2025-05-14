       *> { dg-do run }
       *> { dg-options "-dialect ibm" }
       *> { dg-output-file "group2/IBM_dialect_COMP_redefined_by_POINTER_as_64-bit.out" }

        identification division.
        program-id. prog.
        data division.
        working-storage section.
      *> This is a test of the "-dialect ibm" special interpretation of a common
      *> construction in IBM mainframe code.  That machine is a 32-bit
      *> big-endian architecture.  We are assuming a 64-bit little-endian
      *> x86_64 architecture.  So, the COMP PIC S8(8) would usually be an 32-bit
      *> big-endian value.  But "-dialect ibm" means that the following
      *> REDEFINES USAGE POINTER causes the prior "COMP" to actually be defined
      *> as a 64-bit little-endian binary value.
        77 pointer-value COMP PIC S9(8) VALUE ZERO.
        77 point-at      REDEFINES pointer-value USAGE POINTER.
        procedure division.
      *> The following value is 0x123456789
        move 4886718345 to pointer-value
        display point-at " should be 0x0000000123456789"
        set point-at down by 4886718345
        display point-at " should be 0x0000000000000000"
        set point-at down by 4886718345
        display point-at " should be 0xfffffffedcba9877"
        set point-at up by 4886718345
        display point-at " should be 0x0000000000000000"
        subtract 1 from pointer-value
        display point-at " should be 0xffffffffffffffff"
        add 1 to pointer-value
        display point-at " should be 0x0000000000000000"
        goback.
        end program prog.

