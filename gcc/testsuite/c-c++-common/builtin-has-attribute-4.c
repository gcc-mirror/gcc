/* Verify __builtin_has_attribute return value for variables.
   { dg-do compile }
   { dg-skip-if "No section attribute" { { hppa*-*-hpux* } && { ! lp64 } } }
   { dg-options "-Wall -ftrack-macro-expansion=0" }
   { dg-options "-Wall -Wno-narrowing -Wno-unused -ftrack-macro-expansion=0" { target c++ } }
   { dg-additional-options "-DSKIP_ALIAS" { target *-*-darwin* } }
*/

#define ATTR(...) __attribute__ ((__VA_ARGS__))

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(__builtin_has_attribute (sym, attr) == expect)]

int vnone;

ATTR (aligned) char valigned;
ATTR (aligned (1)) char valigned_1;
ATTR (aligned (2)) char valigned_2;
ATTR (aligned (4)) char valigned_4;
ATTR (aligned (8)) char valigned_8;

void test_aligned (void)
{
  A (0, vnone, aligned);
  A (0, vnone, aligned (0));      /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, vnone, aligned (1));
  A (0, vnone, aligned (2));
  A (0, vnone, aligned (4));
  A (0, vnone, aligned (8));
  A (0, vnone, aligned (16));

  A (1, valigned, aligned);
  A (0, valigned, aligned (0));   /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, valigned, aligned (1));
  A (0, valigned, aligned (2));

  A (1, valigned_1, aligned);
  A (0, valigned_1, aligned (0)); /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (1, valigned_1, aligned (1));
  A (0, valigned_1, aligned (2));
  A (0, valigned_1, aligned (4));

  A (1, valigned_2, aligned);
  A (0, valigned_2, aligned (0)); /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, valigned_2, aligned (1));
  A (1, valigned_2, aligned (2));
  A (0, valigned_2, aligned (4));
}


#ifndef SKIP_ALIAS
int vtarget;
extern ATTR (alias ("vtarget")) int valias;

void test_alias (void)
{
  A (0, vnone, alias);
  A (1, valias, alias);
  A (1, valias, alias ("vtarget"));
  A (0, valias, alias ("vnone"));
}
#endif

void test_cleanup (void)
{
  extern void fpv (void*);
  extern void fcleanup (void*);

  int var;
  ATTR (cleanup (fcleanup)) int var_cleanup;
  A (0, var, cleanup);
  A (1, var_cleanup, cleanup);
  A (1, var_cleanup, cleanup (fcleanup));
  A (0, var_cleanup, cleanup (fpv));
}


ATTR (common) int vcommon;
ATTR (nocommon) int vnocommon;

void test_common (void)
{
  A (0, vnone, common);
  A (0, vnone, nocommon);

  A (1, vcommon, common);
  A (0, vcommon, nocommon);

  A (0, vnocommon, common);
  A (1, vnocommon, nocommon);
}


void test_externally_visible (void)
{
  extern int vexternally_visible;

  A (0, vexternally_visible, externally_visible);

  extern ATTR (externally_visible) int vexternally_visible;

  A (1, vexternally_visible, externally_visible);
}


int test_mode (void)
{
  ATTR (mode (byte)) int i8;
  return __builtin_has_attribute (i8, mode);   /* { dg-warning ".mode. attribute not supported in .__builtin_has_attribute." } */
}


void test_nonstring (void)
{
  char arr[1];
  char* ptr = arr;

  ATTR (nonstring) char arr_nonstring[1];
  ATTR (nonstring) char *ptr_nonstring =  arr_nonstring;

  A (0, arr, nonstring);
  A (0, ptr, nonstring);

  A (1, arr_nonstring, nonstring);
  A (1, ptr_nonstring, nonstring);
}

struct PackedMember
{
  char c;
  short s;
  int i;
  ATTR (packed) int a[2]; /* { dg-warning "attribute ignored" "" { target default_packed } } */
} gpak[2];

void test_packed (struct PackedMember *p)
{
  int vunpacked;
  ATTR (packed) int vpacked;   /* { dg-warning ".packed. attribute ignored" } */

  A (0, vunpacked, packed);
  A (0, vpacked, packed);

  int arr_unpacked[2];
  ATTR (packed) int arr_packed[2];   /* { dg-warning ".packed. attribute ignored" } */

  A (0, arr_unpacked, packed);
  A (0, arr_packed, packed);
  A (0, arr_unpacked[0], packed);
  A (0, arr_packed[0], packed);

  A (0, gpak, packed);
  A (0, gpak[0], packed);
  A (0, *gpak, packed);
  A (0, gpak[0].c, packed);
  A (0, gpak[1].s, packed);
  A (1, gpak->a, packed);
  /* It's the array that's declared packed but not its elements.  */
  A (0, (*gpak).a[0], packed);

  /* The following fails because in C it's represented as
       INDIRECT_REF (POINTER_PLUS (NOP_EXPR (ADDR_EXPR (gpak)), ...))
     with no reference to the member.  Avoid testing it.
  A (1, *gpak[9].a, packed);  */

  A (0, p->c, packed);
  A (0, p->s, packed);
  A (1, p->a, packed);
  /* It's the array that's declared packed but not its elements.  */
  A (0, p->a[0], packed);
  /* Similar to the comment above.
   A (1, *p->a, packed);  */
}


ATTR (section ("sectA")) int var_sectA;
ATTR (section ("sectB")) int var_sectB;

void test_section (void)
{
  int var = 0;
  A (0, var, section);
  A (0, var, section ("sectA"));

  A (1, var_sectA, section);
  A (1, var_sectA, section ("sectA"));
  A (0, var_sectA, section ("sectB"));

  A (1, var_sectB, section);
  A (0, var_sectB, section ("sectA"));
  A (1, var_sectB, section ("sectB"));
}


void test_vector_size (void)
{
  char c;
  extern int arrx[];
  extern int arr1[1];

  A (0, c, vector_size);
  A (0, c, vector_size (1));
  A (0, arrx, vector_size);
  A (0, arrx, vector_size (4));
  A (0, arr1, vector_size);
  A (0, arr1, vector_size (8));

  ATTR (vector_size (4)) char cv4;
  ATTR (vector_size (16)) int iv16;

  A (1, cv4, vector_size);
  A (0, cv4, vector_size (1));
  A (0, cv4, vector_size (2));
  A (1, cv4, vector_size (4));
  A (0, cv4, vector_size (8));

  A (1, iv16, vector_size);
  A (0, iv16, vector_size (1));
  A (0, iv16, vector_size (8));
  A (1, iv16, vector_size (16));
  A (0, iv16, vector_size (32));

  /* Verify that the attribute not detected on an array of vectors
     but is detected on its elements.  */
  typedef ATTR (vector_size (8)) float afv8_t[4];
  A (0, afv8_t, vector_size);
  A (0, afv8_t, vector_size (1));
  A (0, afv8_t, vector_size (2));
  A (0, afv8_t, vector_size (4));
  A (0, afv8_t, vector_size (8));
  A (0, afv8_t, vector_size (16));

  A (1, __typeof__ ((*(afv8_t*)0)[0]), vector_size);
  A (0, __typeof__ ((*(afv8_t*)0)[1]), vector_size (1));
  A (0, __typeof__ ((*(afv8_t*)0)[2]), vector_size (2));
  A (0, __typeof__ ((*(afv8_t*)0)[3]), vector_size (4));
  A (1, __typeof__ ((*(afv8_t*)0)[0]), vector_size (8));
  A (0, __typeof__ ((*(afv8_t*)0)[1]), vector_size (16));

  A (1, __typeof__ (**(afv8_t*)0), vector_size);
  A (0, __typeof__ (**(afv8_t*)0), vector_size (1));
  A (0, __typeof__ (**(afv8_t*)0), vector_size (2));
  A (0, __typeof__ (**(afv8_t*)0), vector_size (4));
  A (1, __typeof__ (**(afv8_t*)0), vector_size (8));
  A (0, __typeof__ (**(afv8_t*)0), vector_size (16));

  ATTR (vector_size (8)) float afv8[4];
  A (0, afv8, vector_size);
  A (0, afv8, vector_size (1));
  A (0, afv8, vector_size (2));
  A (0, afv8, vector_size (4));
  A (0, afv8, vector_size (8));
  A (0, afv8, vector_size (16));

  A (1, afv8[0], vector_size);
  A (0, afv8[1], vector_size (1));
  A (0, afv8[2], vector_size (2));
  A (0, afv8[3], vector_size (4));
  A (1, afv8[0], vector_size (8));
  A (0, afv8[1], vector_size (16));

  A (1, *afv8, vector_size);
  A (0, *afv8, vector_size (1));
  A (0, *afv8, vector_size (2));
  A (0, *afv8, vector_size (4));
  A (1, *afv8, vector_size (8));
  A (0, *afv8, vector_size (16));

  /* sizeof (long double) is 12 on i386.  */
  enum { VecSize = 8 * sizeof (long double) };
  ATTR (vector_size (VecSize)) long double aldv[1][2][3];
  A (0, aldv, vector_size);
  A (0, aldv[0], vector_size);
  A (0, aldv[0][0], vector_size);
  A (1, aldv[0][0][0], vector_size);
  A (0, aldv[0][0][1], vector_size (VecSize / 2));
  A (1, aldv[0][0][2], vector_size (VecSize));

  A (0, aldv[0][0][0][0], vector_size);

  A (0, *aldv, vector_size);
  A (0, **aldv, vector_size);
  A (1, ***aldv, vector_size);
  A (1, ***aldv, vector_size (VecSize));
}


ATTR (visibility ("default")) int vdefault;
ATTR (visibility ("hidden")) int vhidden;
ATTR (visibility ("internal")) int vinternal;
ATTR (visibility ("protected")) int vprotected;

void test_visibility (void)
{
  A (0, vnone, visibility ("default"));
  A (0, vnone, visibility ("hidden"));
  A (0, vnone, visibility ("internal"));
  A (0, vnone, visibility ("protected"));

  A (1, vdefault, visibility ("default"));
  A (0, vdefault, visibility ("hidden"));
  A (0, vdefault, visibility ("internal"));
  A (0, vdefault, visibility ("protected"));

  A (0, vhidden, visibility ("default"));
  A (1, vhidden, visibility ("hidden"));
  A (0, vhidden, visibility ("internal"));
  A (0, vhidden, visibility ("protected"));

  A (0, vinternal, visibility ("default"));
  A (0, vinternal, visibility ("hidden"));
  A (1, vinternal, visibility ("internal"));
  A (0, vinternal, visibility ("protected"));

  A (0, vprotected, visibility ("default"));
  A (0, vprotected, visibility ("hidden"));
  A (0, vprotected, visibility ("internal"));
  A (1, vprotected, visibility ("protected"));
}


int var_init_strong = 123;
int var_uninit_strong;
static int var_extern_strong;
static int var_static_strong;

ATTR (weak) int var_init_weak = 234;
ATTR (weak) int var_uninit_weak;

void test_weak (void)
{
  int var_local = 0;
  static int var_static_local = 0;

  A (0, var_init_strong, weak);
  A (0, var_uninit_strong, weak);
  A (0, var_extern_strong, weak);
  A (0, var_static_strong, weak);
  A (0, var_local, weak);
  A (0, var_static_local, weak);

  A (1, var_init_weak, weak);
  A (1, var_uninit_weak, weak);
} /* { dg-warning "protected visibility attribute not supported" "" { target { *-*-darwin* } } } */

/* { dg-prune-output "specifies less restrictive attribute" } */
