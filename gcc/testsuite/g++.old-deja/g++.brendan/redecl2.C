// GROUPS passed redeclaration
// Check that if multiple declarations of the same single
// function are present in different places in the same file,
// and if these declarations differ (as allowed) in the number
// of argument defaults provided, that correct values are
// passed at all call points anyway.

extern "C" void printf (char *, ...); 

void receiver (int ii, int jj);

void sender_1 ()
{
	receiver (3,7);
}

void receiver (int ii, int jj = 9);

void sender_2 ()
{
	receiver (5);
}

int ii_sum = 0;
int jj_sum = 0;

void sender_3 ();

int main ()
{
	sender_1 ();
	sender_2 ();
	sender_3 ();
	if (ii_sum != 13 || jj_sum != 25)
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");

	return 0;
}

void receiver (int ii, int jj)
{
	ii_sum += ii;
	jj_sum += jj;
}

void sender_3 ()
{
	receiver (5);
}
