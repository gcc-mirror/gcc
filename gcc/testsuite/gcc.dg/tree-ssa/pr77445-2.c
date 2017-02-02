/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread-details-blocks-stats" } */
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
   increase much.  */
/* { dg-final { scan-tree-dump "Jumps threaded: 1\[1-9\]" "thread1" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum" 2 "thread1" } } */
/* { dg-final { scan-tree-dump-not "not considered" "thread1" } } */
/* { dg-final { scan-tree-dump-not "not considered" "thread2" } } */
/* { dg-final { scan-tree-dump-not "not considered" "thread3" } } */
/* { dg-final { scan-tree-dump-not "not considered" "thread4" } } */
