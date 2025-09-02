/* { dg-do compile } */
/* { dg-options "-std=gnu17" } */

typedef union{
        char *nordic_ref;
        unsigned long long int bit_number;
        enum PinMode mode : 2;		/* { dg-warning "narrower" } */
					/* { dg-error "field 'mode'" "" { target *-*-* } .-1 } */
        unsigned char value;
    } s; typedef struct{
        union{
            char *nordic_ref;
            unsigned long long int bit_number;
            enum PinMode mode : 2;	/* { dg-warning "narrower" } */
					/* { dg-error "field 'mode'" "" { target *-*-* } .-1 } */
            unsigned char value;
        } s;
}					/* { dg-error "expected identifier" } */

