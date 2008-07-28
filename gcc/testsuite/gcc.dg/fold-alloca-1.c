/* { dg-do compile } */
/* { dg-options "-fdump-tree-cleanup_cfg1" } */

void *alloca (__SIZE_TYPE__);
void link_error ();

int main (int argc, char *argv[]) {
	char *foo;
	if ((foo = alloca(argc)) == 0)
	  link_error ();
	return 0;
}
/* { dg-final { scan-tree-dump-times "link_error" 0 "cleanup_cfg1" } } */
/* { dg-final { cleanup-tree-dump "cleanup_cfg1" } } */
