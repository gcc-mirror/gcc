/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-thread-details-blocks-stats -fdump-tree-threadfull1-blocks-stats -fdump-tree-threadfull2-blocks-stats" } */
typedef enum STATES {
	START=0,
	INVALID,
	S1,
	S2,
	INT,
	FLOAT ,
	EXPONENT,
	SCIENTIFIC,
	NUM_STATES
} state_e ;

typedef unsigned char u8;
typedef unsigned int  u32;

static u8 is_digit(u8 c) {
	return (u8)((c>='0') & (c<='9')) ? 1 : 0;
}

enum STATES FMS( u8 **in , u32 *transitions) {
	u8 *str = *in;
	u8 NEXT_SYMBOL;
	enum STATES state=START;
	for( ; *str && state != INVALID; str++ ) {
		NEXT_SYMBOL = *str;
		if (NEXT_SYMBOL==',') /* end of this input */ {
			transitions[state]++;
			str++;
			break;
		}
		switch(state) {
		case START:
			if(is_digit(NEXT_SYMBOL)) {
				state = INT;
			}
			else if( NEXT_SYMBOL == '+' || NEXT_SYMBOL == '-' ) {
				state = S1;
			}
			else if( NEXT_SYMBOL == '.' ) {
				state = FLOAT ;
			}
			else {
				state = INVALID;
			}
			transitions[START]++;
			break;
		case S1:
			if(is_digit(NEXT_SYMBOL)) {
				state = INT;
				transitions[S1]++;
			}
			else if( NEXT_SYMBOL == '.' ) {
				state = FLOAT ;
				transitions[S1]++;
			}
			else {
				state = INVALID;
				transitions[S1]++;
			}
			break;
		case INT:
			if( NEXT_SYMBOL == '.' ) {
				state = FLOAT ;
				transitions[INT]++;
			}
			else if(!is_digit(NEXT_SYMBOL)) {
				state = INVALID;
				transitions[INT]++;
			}
			break;
		case FLOAT :
			if( NEXT_SYMBOL == 'E' || NEXT_SYMBOL == 'e' ) {
				state = S2;
				transitions[FLOAT ]++;
			}
			else if(!is_digit(NEXT_SYMBOL)) {
				state = INVALID;
				transitions[FLOAT ]++;
			}
			break;
		case S2:
			if( NEXT_SYMBOL == '+' || NEXT_SYMBOL == '-' ) {
				state = EXPONENT;
				transitions[S2]++;
			}
			else {
				state = INVALID;
				transitions[S2]++;
			}
			break;
		case EXPONENT:
			if(is_digit(NEXT_SYMBOL)) {
				state = SCIENTIFIC;
				transitions[EXPONENT]++;
			}
			else {
				state = INVALID;
				transitions[EXPONENT]++;
			}
			break;
		case SCIENTIFIC:
			if(!is_digit(NEXT_SYMBOL)) {
				state = INVALID;
			}
			break;
		default:
			break;
		}
	}
	if (state==INVALID)
		transitions[INVALID]++;
	
	*in = str;
	return state;
}

/* The profile is not updated perfectly because it is inconsitent from
   profile estimation stage. But the number of inconsistencies should not
   increase much. 

   aarch64 has the highest CASE_VALUES_THRESHOLD in GCC.  It's high enough
   to change decisions in switch expansion which in turn can expose new
   jump threading opportunities.  Skip the later tests on aarch64.  */
/* { dg-final { scan-tree-dump "Jumps threaded: \[7-9\]" "thread1" } } */
/* { dg-final { scan-tree-dump-not "optimizing for size" "thread1" } } */
/* { dg-final { scan-tree-dump-not "optimizing for size" "threadfull1" } } */
/* { dg-final { scan-tree-dump-not "optimizing for size" "thread2" { target { ! aarch64*-*-* } } } } */
/* { dg-final { scan-tree-dump-not "optimizing for size" "threadfull2" { target { ! aarch64*-*-* } } } } */ 
