/* PR c/81417 */
/* { dg-do compile } */
/* { dg-options "-Wsign-compare -fdiagnostics-show-caret" } */

unsigned int
f0 (int x, unsigned int y)
{
  return x ? y : -1; /* { dg-warning "18:operand of '\\?:' changes signedness from 'int' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return x ? y : -1;
                  ^~
   { dg-end-multiline-output "" } */
}

unsigned int
f1 (int xxx, unsigned int yyy)
{
  return xxx ? yyy : -1; /* { dg-warning "22:operand of '\\?:' changes signedness from 'int' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return xxx ? yyy : -1;
                      ^~
   { dg-end-multiline-output "" } */
}

unsigned int
f2 (int xxx, unsigned int yyy)
{
  return xxx ? -1 : yyy; /* { dg-warning "16:operand of '\\?:' changes signedness from 'int' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return xxx ? -1 : yyy;
                ^~
   { dg-end-multiline-output "" } */
}

unsigned int
f3 (unsigned int yyy)
{
  return yyy ?: -1; /* { dg-warning "17:operand of '\\?:' changes signedness from 'int' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return yyy ?: -1;
                 ^~
   { dg-end-multiline-output "" } */
}

unsigned int
f4 (int xxx, unsigned yyy, short uuu)
{
  return xxx ? yyy : uuu; /* { dg-warning "22:operand of '\\?:' changes signedness from 'short int' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return xxx ? yyy : uuu;
                      ^~~
   { dg-end-multiline-output "" } */
}

unsigned int
f5 (int xxx, unsigned yyy, short uuu)
{
  return xxx ? uuu : yyy; /* { dg-warning "16:operand of '\\?:' changes signedness from 'short int' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return xxx ? uuu : yyy;
                ^~~
   { dg-end-multiline-output "" } */
}

unsigned int
f6 (int xxx, unsigned yyy, signed char uuu)
{
  return xxx ? yyy : uuu; /* { dg-warning "22:operand of '\\?:' changes signedness from 'signed char' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return xxx ? yyy : uuu;
                      ^~~
   { dg-end-multiline-output "" } */
}

unsigned int
f7 (int xxx, unsigned yyy, signed char uuu)
{
  return xxx ? uuu : yyy; /* { dg-warning "16:operand of '\\?:' changes signedness from 'signed char' to 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   return xxx ? uuu : yyy;
                ^~~
   { dg-end-multiline-output "" } */
}
