/* PR middle-end/10138 - warn for uninitialized arrays passed as const arguments
   Verify that -Wuninitialized and -Wmaybe-uninitialized trigger (or don't)
   when passing uninitialized variables by reference to functions declared
   with or without attribute access and with (or without) const qualified
   arguments of array, VLA, or pointer types.
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0 -ftrivial-auto-var-init=zero" } */

#define NONE    /* none */
#define RO(...) __attribute__ ((access (read_only, __VA_ARGS__)))
#define RW(...) __attribute__ ((access (read_write, __VA_ARGS__)))
#define WO(...) __attribute__ ((access (write_only, __VA_ARGS__)))
#define X(...)  __attribute__ ((access (none, __VA_ARGS__)))

#define CONCAT(x, y) x ## y
#define CAT(x, y)    CONCAT (x, y)
#define UNIQ(pfx)    CAT (pfx, __LINE__)

extern void sink (void*);


#define T1(attr, name, type)			\
  void UNIQ (CAT (test_, name))(void) {		\
    extern attr void UNIQ (name)(type);		\
    int x;					\
    UNIQ (name)(&x);				\
    sink (&x);					\
  }

#define T2(attr, name, types)			\
  void UNIQ (CAT (test_, name))(void) {		\
    extern attr void UNIQ (name)(types);	\
    int x;					\
    UNIQ (name)(1, &x);				\
    sink (&x);					\
  }


typedef int IA_[];
typedef const int CIA_[];

T1 (NONE,   fia_,   IA_);
T1 (NONE,   fcia_,  CIA_);    // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (RO (1), froia_, IA_);     // { dg-warning "\\\[-Wuninitialized" "" }
T1 (RW (1), frwia_, IA_);     // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (WO (1), fwoia_, IA_);
T1 (X (1),  fxia_,  IA_);


typedef int IA1[1];
typedef const int CIA1[1];

T1 (NONE,   fia1,   IA1);
T1 (NONE,   fcia1,  CIA1);    // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (RO (1), froia1, IA1);     // { dg-warning "\\\[-Wuninitialized" "" }
T1 (RW (1), frwia1, IA1);     // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (WO (1), fwoia1, IA1);
T1 (X (1),  fxia1,  IA1);


#define IARS1  int[restrict static 1]
#define CIARS1 const int[restrict static 1]

T1 (NONE,   fiars1,   IARS1);
T1 (NONE,   fciars1,  CIARS1);// { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (RO (1), froiars1, IARS1); // { dg-warning "\\\[-Wuninitialized" "" }
T1 (RW (1), frwiars1, IARS1); // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (WO (1), fwoiars1, IARS1);
T1 (X (1),  fxiars1,  IARS1);


#define IAS1  int[static 1]
#define CIAS1 const int[static 1]

T1 (NONE,   fias1,   IAS1);
T1 (NONE,   fcias1,  CIAS1);   // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (RO (1), froias1, IAS1);    // { dg-warning "\\\[-Wuninitialized" "" }
T1 (RW (1), frwias1, IAS1);    // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (WO (1), fwoias1, IAS1);
T1 (X (1),  fxias1,  IAS1);


#define IAX  int[*]
#define CIAX const int[*]

T1 (NONE,   fiax,   IAX);
T1 (NONE,   fciax,  CIAX);    // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (RO (1), froiax, IAX);     // { dg-warning "\\\[-Wuninitialized" "" }
T1 (RW (1), frwiax, IAX);     // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (WO (1), fwoiax, IAX);
T1 (X (1),  fxiax,  IAX);


#define IAN  int n, int[n]
#define CIAN int n, const int[n]

T2 (NONE,      fian,   IAN);
T2 (NONE,      fcian,  CIAN); // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T2 (RO (2, 1), froian, IAN);  // { dg-warning "\\\[-Wuninitialized" "" }
T2 (RW (2, 1), frwian, IAN);  // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T2 (WO (2, 1), fwoian, IAN);
T2 (X (2, 1),  fxian,  IAN);


typedef int* IP;
typedef const int* CIP;

T1 (NONE,   fip,   IP);
T1 (NONE,   fcip,  CIP);     // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (RO (1), froip, IP);      // { dg-warning "\\\[-Wuninitialized" "" }
T1 (RW (1), frwip, IP);      // { dg-warning "\\\[-Wmaybe-uninitialized" "" }
T1 (WO (1), fwoip, IP);
T1 (X (1),  fxip,  IP);


/* Verify that the notes printed after the warning mention attribute
   access only when the attribute is explicitly used in the declaration
   and not otherwise.  */

void test_note_cst_restrict (void)
{
  extern void
    fccar (const char[restrict]);   // { dg-message "by argument 1 of type 'const char\\\[restrict]' to 'fccar'" "note" }

  char a[1];                  // { dg-message "'a' declared here" "note" }
  fccar (a);                  // { dg-warning "'a' may be used uninitialized" }
}

void test_note_vla (int n)
{
  extern void
    fccvla (const char[n]);   // { dg-message "by argument 1 of type 'const char\\\[n]' to 'fccvla'" "note" }

  char a[2];                  // { dg-message "'a' declared here" "note" }
  fccvla (a);                 // { dg-warning "'a' may be used uninitialized" }
}

void test_note_ro (void)
{
  extern RO (1) void
    frocar (char[restrict]);  // { dg-message "in a call to 'frocar' declared with attribute 'access \\\(read_only, 1\\\)'" "note" }

  char a[3];                  // { dg-message "'a' declared here" "note" }
  frocar (a);                 // { dg-warning "'a' is used uninitialized" }
}

void test_note_rw (void)
{
  extern RW (1) void
    frwcar (char[restrict]);  // { dg-message "in a call to 'frwcar' declared with attribute 'access \\\(read_write, 1\\\)'" "note" }

  char a[4];                  // { dg-message "'a' declared here" "note" }
  frwcar (a);                 // { dg-warning "'a' may be used uninitialized" }
}
