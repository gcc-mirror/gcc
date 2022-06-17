/* { dg-do compile } */
static inline void sub_1 ( ) { 
	struct struct_1 var_9 , var_10
}

static int var_9[1] __attribute__ ( ( section ( ".data" ) ) ) ; 
/* { dg-excess-errors "" } */
