/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O0 -fdump-tree-tmmark-lineno" } */

/* Test that instrumented statements have correct location info.  */

int a,b, c, z;

testing(){
    c=9;
}

main() {
	b = 9898;
	__transaction_relaxed {
		z = c;
		a = 888;
		testing();
	}
	return 0;
}

/* { dg-final { scan-tree-dump-times ": 13:.*b = 9898" 1 "tmmark" } } */
/* { dg-final { scan-tree-dump-times ": 14:.*__transaction" 1 "tmmark" } } */
/* { dg-final { scan-tree-dump-times ": 15:.*ITM_WU. \\(&z" 1 "tmmark" } } */
/* { dg-final { scan-tree-dump-times ": 16:.*ITM_WU. \\(&a" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
