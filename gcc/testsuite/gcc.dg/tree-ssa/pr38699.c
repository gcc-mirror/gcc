/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */
/*
 * License: GPL
 * Copyright: (c) 2007 by Till Harbaum <till@harbaum.org>
 * Copyright: (c) 2008 Wolfgang Moser, http://d81.de
 */

/* #include <avr/wdt.h> */
/* some declarations from the include above for ATMega8 */
#define _SFR_IO8(io_addr) (*(volatile unsigned char *)((io_addr) + 0x20))
#define TOV0    0
#define TCNT0    _SFR_IO8(0x32)
#define TIFR    _SFR_IO8(0x38)
#define DDRC    _SFR_IO8(0x14)
#define DDRD    _SFR_IO8(0x11)
#define PORTC    _SFR_IO8(0x15)
#define PORTD    _SFR_IO8(0x12)


static void delay_wait_us( unsigned char timeout ) {
    __asm__ __volatile__ ("wdr");

    TCNT0 = timeout;
    TIFR |= (1 << (TOV0));

    /* wait until counter overflows */
    while(!(TIFR & (1 << (TOV0))));
}

/* The original testcase was multiplying by 1000.  Gcc is now smart enough
   to work out that actual parameter is 5000 that is not what testcase was
   about.  Obstructate the code somewhat then.  */
int a;
static void delay_wait_us_ms( unsigned char timeout ) {
    delay_wait_us( timeout * a );
}


void makeUseOfTimerWait( unsigned char val ) {
    delay_wait_us( 10 );
    DDRC |= 0x10;
    delay_wait_us( 10 );
    DDRD |= 0x20;

    delay_wait_us( 20 );
    PORTC &= ~0x10;
    delay_wait_us( 10 );
    PORTD &= ~0x20;

    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );
    delay_wait_us_ms( 5 );

    PORTC |= val & 0x10;
    delay_wait_us( 10 );
    PORTD |= val & 0x20;

    delay_wait_us( 10 );
}
/* We should not inline the delay loops.  */
/* { dg-final { scan-tree-dump "delay_wait_us" "optimized"} } */
/* { dg-final { scan-tree-dump "delay_wait_us_ms" "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
