/* { dg-do compile } */
/* From a failure after the global ccp pass.  */
typedef struct
{
  char n[129];
} A;

const A C = {
  0,
  0
};

extern const A *const B;

void bar(const char *);

void foo ()
{
  bar (B->n);
}

const A *const B = &C;
