/* PR tree-optimization/91294 - wrong strlen result of a conditional with
   an offset
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define NOIPA __attribute__ ((noclone, noinline, noipa))

#define CAT(a, b) a ## b
#define CONCAT(a, b) CAT (a, b)
#define UNIQ_NAME(name) CONCAT (name, __LINE__)

extern int last_line;
int nfails;

char buf[32];

#define VERIFY(expr, nbytes, expect)					\
  NOIPA void UNIQ_NAME (test_)(void)					\
  {									\
    memcpy (buf, (expr), (nbytes));					\
    const size_t len = strlen (buf);					\
    if (len != expect)							\
      {									\
	++nfails;							\
	__builtin_printf ("line %i: strlen(%s) == %zu failed: "		\
			  "got %zu\n",					\
			  __LINE__ - 1000 + last_line + 2,		\
			  #expr, (size_t)expect,			\
			  len);						\
      }									\
  } typedef void DummyType

const char a8[12] = "01234567";
const char b8[12] = "76543210";
const char c4[12] = "0123";

int i0, i1 = 1, i2 = 2;

int last_line = __LINE__;
#line 1000
VERIFY (i0 ? (a8 + 0) : (b8 + 0), 9, 8);
VERIFY (i0 ? (a8 + 0) : (b8 + 1), 8, 7);
VERIFY (i0 ? (a8 + 0) : (b8 + 2), 8, 6);
VERIFY (i0 ? (a8 + 0) : (b8 + 2), 7, 6);
VERIFY (i0 ? (a8 + 0) : (b8 + 3), 8, 5);
VERIFY (i0 ? (a8 + 0) : (b8 + 3), 7, 5);
VERIFY (i0 ? (a8 + 0) : (b8 + 3), 6, 5);
VERIFY (i0 ? (a8 + 0) : (b8 + 4), 8, 4);
VERIFY (i0 ? (a8 + 0) : (b8 + 4), 7, 4);
VERIFY (i0 ? (a8 + 0) : (b8 + 4), 6, 4);
VERIFY (i0 ? (a8 + 0) : (b8 + 4), 5, 4);
VERIFY (i0 ? (a8 + 0) : (b8 + 5), 7, 3);
VERIFY (i0 ? (a8 + 0) : (b8 + 5), 6, 3);
VERIFY (i0 ? (a8 + 0) : (b8 + 5), 5, 3);
VERIFY (i0 ? (a8 + 0) : (b8 + 5), 4, 3);
VERIFY (i0 ? (a8 + 0) : (b8 + 6), 3, 2);
VERIFY (i0 ? (a8 + 0) : (b8 + 7), 2, 1);
VERIFY (i0 ? (a8 + 1) : (b8 + 0), 8, 8);
VERIFY (i0 ? (a8 + 2) : (b8 + 0), 7, 7);
VERIFY (i0 ? (a8 + 1) : (b8 + 1), 8, 7);
VERIFY (i0 ? (a8 + 1) : (b8 + 2), 7, 6);
VERIFY (i0 ? (a8 + 2) : (b8 + 1), 8, 7);   // FAIL
VERIFY (i0 ? (a8 + 2) : (b8 + 2), 7, 6);
VERIFY (i0 ? (a8 + 0) : (b8 + 0), 9, 8);
VERIFY (i0 ? (a8 + 0) : (b8 + 1), 8, 7);
VERIFY (i0 ? (a8 + 0) : (b8 + 2), 7, 6);
VERIFY (i0 ? (a8 + 1) : (b8 + 0), 9, 8);
VERIFY (i0 ? (a8 + 2) : (b8 + 0), 9, 8);
VERIFY (i0 ? (a8 + 1) : (b8 + 1), 8, 7);
VERIFY (i0 ? (a8 + 1) : (b8 + 2), 7, 6);
VERIFY (i0 ? (a8 + 2) : (b8 + 1), 8, 7);   // FAIL
VERIFY (i0 ? (a8 + 2) : (b8 + 2), 7, 6);
VERIFY (i0 ? (a8 + 0) : (c4 + 0), 9, 4);
VERIFY (i0 ? (a8 + 0) : (c4 + 1), 9, 3);
VERIFY (i0 ? (a8 + 0) : (c4 + 3), 9, 1);
VERIFY (i0 ? (a8 + 0) : (c4 + 4), 9, 0);
VERIFY (i0 ? (a8 + 1) : (c4 + 0), 8, 4);
VERIFY (i0 ? (a8 + 1) : (c4 + 1), 8, 3);
VERIFY (i0 ? (a8 + 1) : (c4 + 2), 8, 2);
VERIFY (i0 ? (a8 + 1) : (c4 + 3), 8, 1);
VERIFY (i0 ? (a8 + 1) : (c4 + 4), 8, 0);
VERIFY (i0 ? (a8 + 2) : (c4 + 0), 8, 4);
VERIFY (i0 ? (a8 + 2) : (c4 + 1), 8, 3);
VERIFY (i0 ? (a8 + 2) : (c4 + 2), 8, 2);
VERIFY (i0 ? (a8 + 2) : (c4 + 3), 8, 1);
VERIFY (i0 ? (a8 + 2) : (c4 + 4), 8, 0);
VERIFY ((i0 ? a8 : b8) + 1, 8, 7);
VERIFY ((i0 ? a8 : b8) + 2, 8, 6);
VERIFY ((i0 ? a8 : b8) + 2, 7, 6);
VERIFY ((i0 ? a8 : b8) + 3, 3, 3);
VERIFY ((i0 ? a8 : b8) + 3, 1, 1);
VERIFY ((i0 ? a8 : c4) + 1, 8, 3);
VERIFY ((i0 ? a8 : c4) + 3, 8, 1);
VERIFY ((i0 ? a8 : c4) + 4, 8, 0);
VERIFY ((i0 ? a8 + 1: b8 + 2) + 1, 9, 5);
VERIFY ((i0 ? a8 + i1: b8 + i2) + 1, 8, 5);
VERIFY ((i0 ? a8 + i1: b8 + 2) + 1, 8, 5);
VERIFY ((i0 ? a8 + i2: b8 + i1) + 1, 8, 6);
VERIFY ((i0 ? a8 + 2: b8 + i1) + 1, 8, 6);

#define T(N) test_ ## N (); memset (buf, 0, sizeof buf)

int main (void)
{
  T (1000);
  T (1001);
  T (1002);
  T (1003);
  T (1004);
  T (1005);
  T (1006);
  T (1007);
  T (1008);
  T (1009);

  T (1010);
  T (1011);
  T (1012);
  T (1013);
  T (1014);
  T (1015);
  T (1016);
  T (1017);
  T (1018);
  T (1019);

  T (1020);
  T (1021);
  T (1022);
  T (1023);
  T (1024);
  T (1025);
  T (1026);
  T (1027);
  T (1028);
  T (1029);

  T (1030);
  T (1031);
  T (1032);
  T (1033);
  T (1034);
  T (1035);
  T (1036);
  T (1037);
  T (1038);
  T (1039);

  T (1040);
  T (1041);
  T (1042);
  T (1043);
  T (1044);
  T (1045);
  T (1046);
  T (1047);
  T (1048);
  T (1049);

  T (1050);
  T (1051);
  T (1052);
  T (1053);
  T (1054);
  T (1055);
  T (1056);
  T (1057);
  T (1058);

  if (nfails)
    abort ();
}

