/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-release_ssa"  } */
struct wrap {
	void **array;
};
__attribute__ ((noinline))
void
write_array (struct wrap *ptr)
{
	ptr->array[0]=0;
}
int
test ()
{
	void *arrayval;
	struct wrap w = {&arrayval};
	write_array (&w);
	return w.array == &arrayval;
}
/* We should deterine that write_array writes to PTR only indirectly.  */
/* { dg-final { scan-tree-dump "return 1" "release_ssa"  } } */
