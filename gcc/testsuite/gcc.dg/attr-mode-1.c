/* PR c/17384 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef struct __attribute__((mode(SI))) { 
    unsigned    INT0    :1, 
                RES0    :1, 
                        :6, 
                INT1    :1, 
                RES1    :1, 
                        :6, 
                        :16; 
} MCR;  /* { dg-error "inappropriate type" } */
