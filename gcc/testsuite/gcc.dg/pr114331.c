/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int src(int num) {
    switch((short)num){
        case 111:
	  /* Should fold to 110.  */
          return num & 0xfffe;
        case 267:
        case 204:
        case 263:
          return 0;
        default:
          return 0;
    }
}


/* { dg-final { scan-tree-dump "110"  "optimized" } } */

