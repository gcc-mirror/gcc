/* PR c/5503
   Test whether argument checking is done for fputs, bzero and bcmp.  */
typedef struct { int i; } FILE;
typedef __SIZE_TYPE__ size_t;
int fputs (const char *, FILE *);
void bzero (void *, size_t);
int bcmp (const void *, const void *, size_t);

char buf[32];
FILE *f;

int main ()
{
  fputs ("foo");		/* { dg-error "too few" } */
  fputs ("foo", "bar", "baz");	/* { dg-error "too many" } */
  fputs (21, 43);
  bzero (buf);			/* { dg-error "too few" } */
  bzero (21);			/* { dg-error "too few" } */
  bcmp (buf, buf + 16);		/* { dg-error "too few" } */
  bcmp (21);			/* { dg-error "too few" } */
  fputs ("foo", f);
  bzero (buf, 32);
  bcmp (buf, buf + 16, 16);
  return 0;
}

/* { dg-warning "passing argument 2 of" "2nd incompatible" { target *-*-* } 15 } */
/* { dg-warning "passing argument 1 of" "1st incompatible" { target *-*-* } 16 } */
/* { dg-warning "passing argument 2 of" "2nd incompatible" { target *-*-* } 16 } */
/* { dg-warning "passing argument 1 of" "1st incompatible" { target *-*-* } 18 } */
/* { dg-warning "passing argument 1 of" "1st incompatible" { target *-*-* } 20 } */
