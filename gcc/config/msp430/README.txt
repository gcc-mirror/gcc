Random Notes
------------

The MSP430 port does not use leading underscores.  However, the
assembler has no way of differentiating between, for example, register
R12 and symbol R12.  So, if you do "int r12;" in your C program, you
may get an assembler error, and will certainly have runtime problems.
