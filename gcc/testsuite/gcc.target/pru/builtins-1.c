/* { dg-do compile } */

void delay0 (void)  { __delay_cycles (0); }
void delay1 (void)  { __delay_cycles (1); }
void delay2 (void)  { __delay_cycles (2); }
void delay3 (void)  { __delay_cycles (3); }

void delay_1 (void)  { __delay_cycles (44); }
void delay_2 (void)  { __delay_cycles (0x1234); }
void delay_3 (void)  { __delay_cycles (0x123456); }

/* { dg-final { scan-assembler-not "__delay_cycles" } } */
