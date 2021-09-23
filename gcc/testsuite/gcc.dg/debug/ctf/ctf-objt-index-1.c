/* CTF objext index sub-section.

   An object index sub-section in the CTF section contains the offset to the
   string name of the global object symbols.  The number of entries in the
   obj info section and objt index section are always the same. 

   In this testcase, 4 records in the object index section are expected.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "objtinfo_name" 4 } } */
/* { dg-final { scan-assembler-times "objtinfo_var_type" 4 } } */
/* { dg-final { scan-assembler-times "ascii \"a.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"b.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"a1.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"d_instance.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

static int b = 33;

int a = 44;
int a1[2] = {22, 33};

struct d
{
  int d1;
  int d2;
};

struct d d_instance;
