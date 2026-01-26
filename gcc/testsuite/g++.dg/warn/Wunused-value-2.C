// { dg-do compile }
// { dg-options "-Wall" }

static int index() { return 0; }

volatile int global;
static int index_with_side_effect() {
    global += 1;
    return 0;
}

int main(int argc, char **argv)
{
	const bool cond = argc == 10;
	(void)(cond ? "" : "")[index()];
	(void)(cond ? "" : "")[index_with_side_effect()];
	(void)(cond ? "" : "")[(1, 0)]; // { dg-warning "left operand of comma operator has no effect" }
}
