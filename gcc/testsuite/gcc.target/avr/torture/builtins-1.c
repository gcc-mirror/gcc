/* { dg-do compile } */

void nop (void)    { __builtin_avr_nop (); }
void sei (void)    { __builtin_avr_sei (); }
void cli (void)    { __builtin_avr_cli (); }
void wdr (void)    { __builtin_avr_wdr (); }
void sleep (void)  { __builtin_avr_sleep (); }

char fmul (char a, char b)   { return __builtin_avr_fmul (a, b); }
char fmuls (char a, char b)  { return __builtin_avr_fmuls (a, b); }
char fmulsu (char a, char b) { return __builtin_avr_fmulsu (a, b); }

char swap1 (char a)
{
    return __builtin_avr_swap (a+1);
}

char swap2 (char a)
{
    return __builtin_avr_swap (__builtin_avr_swap (a+1));
}

char swap15 (void)
{
    return __builtin_avr_swap (15);
}

void delay0 (void)  { __builtin_avr_delay_cycles (0); }
void delay1 (void)  { __builtin_avr_delay_cycles (1); }
void delay2 (void)  { __builtin_avr_delay_cycles (2); }
void delay3 (void)  { __builtin_avr_delay_cycles (3); }

void delay_1 (void)  { __builtin_avr_delay_cycles (44); }
void delay_2 (void)  { __builtin_avr_delay_cycles (0x1234); }
void delay_3 (void)  { __builtin_avr_delay_cycles (0x123456); }
void delay_4 (void)  { __builtin_avr_delay_cycles (-1ul); }

/* { dg-final { scan-assembler-not "__builtin_avr_" } } */
