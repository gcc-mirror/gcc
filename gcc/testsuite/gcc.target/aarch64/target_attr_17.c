__attribute__((target("invalid-attr-string")))
int
foo (int a)
{
  return a + 5;
}

/* { dg-error "attribute 'target\\(\"invalid-attr-string\"\\)' is not valid" "" { target *-*-* } 0 } */
