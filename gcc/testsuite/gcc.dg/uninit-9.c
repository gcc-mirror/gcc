/* Spurious uninitialized variable warnings.  Slight variant on the
   documented case, inspired by reg-stack.c:record_asm_reg_life.  */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

struct foo
{
    int type;
    struct foo *car;
    struct foo *cdr;
    char *data;
    int data2;
};

extern void use(struct foo *);

#define CLOBBER 6
#define PARALLEL 3

void
func(struct foo *list, int count)
{
    int n_clobbers = 0;
    int i;
    struct foo **clob_list;   /* { dg-bogus "clob_list" "uninitialized variable warning" } */

    if(list[0].type == PARALLEL)
    {
	clob_list = __builtin_alloca(count * sizeof(struct foo *));
	
	for(i = 1; i < count; i++)
	{
	    if(list[i].type == CLOBBER)
		clob_list[n_clobbers++] = &list[i];
	}
    }

    for(i = 0; i < n_clobbers; i++)
	use(clob_list[i]);
}
