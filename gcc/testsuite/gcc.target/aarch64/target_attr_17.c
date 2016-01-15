__attribute__((target("invalid-attr-string")))
int
foo (int a)
{
  return a + 5;
}

/* { dg-error "target attribute.*is invalid" "" { target *-*-* } 0 } */