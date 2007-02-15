/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-cfg" } */
/* { dg-skip-if "" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */
double a;
void t()
{
	a=1;
}
void t1(void);
void abort(void);

void q()
{
	try {
		t();
	}
	catch (...) {abort();}
}
/* We shouldnotice nothrow attribute.  */
/* { dg-final { scan-tree-dump-times "exception" 0 "cfg"} } */
/* { dg-final { cleanup-tree-dump "cfg" } } */
