// { dg-do run }
// { dg-options "-O -fsanitize=null -fno-sanitize-recover=null -fdump-tree-sanopt-details" }
// { dg-skip-if "" { *-*-* } { "-flto -fno-fat-lto-objects" } }
// { dg-shouldfail "ubsan" }

int
main (void)
{
  int *p = 0;

  int &r1 = *p;
  int &r2 = *p;
  int &r3 = *p;
  int &r4 = *p;
  int &r5 = *p;
}

// { dg-output "reference binding to null pointer of type 'int'" }
// { dg-final { scan-tree-dump-times "Optimizing" 4 "sanopt"} }
