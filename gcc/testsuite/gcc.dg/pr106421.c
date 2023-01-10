/* { dg-do compile } */
/* { dg-options "-O2" } */

int main(int argc, char **argv)
{
	__label__ loop, end;
	void jmp(int c) { goto *(c ? &&loop : &&end); }
loop:
	jmp(argc < 0);
end:
	return 0;
}

