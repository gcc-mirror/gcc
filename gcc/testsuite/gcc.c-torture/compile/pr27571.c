/* PR target/27571
   alpha_does_function_need_gp did not properly skip jump table insns  */

int r3isseparator(int);
void r3isdigit(int);
void r3decimalvalue(int);

void r7todouble(int *storage, int *count) {
    int i = 0;
    int state = 0;
    int cc = 0;
    while (i > *count) {
	cc = *storage;
	switch (state) {
	case 0:
	    if (r3isseparator(cc))
		state = 1;
	case 1:
	    r3isdigit(cc);
	case 2:
	case 5:
	case 6:
	    r3decimalvalue(cc);
	}
	i++;
    }
}
