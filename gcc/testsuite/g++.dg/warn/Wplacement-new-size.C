/* { dg-do compile } */
/* { dg-options "-Wplacement-new -fpermissive" } */

typedef __typeof__ (sizeof 0) size_t;

void* operator new (size_t, void *p) { return p; }
void* operator new[] (size_t, void *p) { return p; }

static __attribute__ ((used))char c;
static __attribute__ ((used))char ac1 [1];
static __attribute__ ((used))char ac2 [2];
static __attribute__ ((used))char ac3 [3];
static __attribute__ ((used))char ac4 [4];
static __attribute__ ((used))char ac5 [5];
static __attribute__ ((used))char ac6 [6];
static __attribute__ ((used))char ac7 [7];
static __attribute__ ((used))char ac8 [8];

static __attribute__ ((used))char ac1_1 [1][1];
static __attribute__ ((used))char ac1_2 [1][2];
static __attribute__ ((used))char ac2_1 [2][1];
static __attribute__ ((used))char ac2_2 [2][2];

static __attribute__ ((used))short s;
static __attribute__ ((used))short as1 [1];
static __attribute__ ((used))short as2 [2];

static __attribute__ ((used))struct SC { char c; char *pc; void *pv; } sc;
static __attribute__ ((used))struct SAC1 { char ac [1]; } sac1;
static __attribute__ ((used))struct SAC2 { char ac [2]; } sac2;
static __attribute__ ((used))struct SAC3 { char ac [3]; } sac3;
static __attribute__ ((used))struct SAC4 { char ac [4]; } sac4;

static __attribute__ ((used))struct SSC { SC sc; int x; } ssc;
static __attribute__ ((used))struct SSAC1 { SAC1 sac; } ssac1;
static __attribute__ ((used))struct SSAC2 { SAC2 sac; } ssac2;
static __attribute__ ((used))struct SSAC3 { SAC3 sac; } ssac3;
static __attribute__ ((used))struct SSAC4 { SAC4 sac; } ssac4;

static __attribute__ ((used))struct SSAC4_2 { SSAC4 ssac4_2 [2]; } sssac4_2;

static __attribute__ ((used))union UAC1 { char c; char ac [1]; } uac1;
static __attribute__ ((used))union UAC2 { char c; char ac [2]; } uac2;
static __attribute__ ((used))union UAC3 { char c; char ac [3]; } uac3;
static __attribute__ ((used))union UAC4 { char c; char ac [4]; } uac4;

static __attribute__ ((used))SC fsc ()  { return SC (); }
static __attribute__ ((used))SAC1 fasc1 () { return SAC1 (); }
static __attribute__ ((used))SAC2 fasc2 () { return SAC2 (); }
static __attribute__ ((used))SAC3 fasc3 () { return SAC3 (); }
static __attribute__ ((used))SAC4 fasc4 () { return SAC4 (); }

static __attribute__ ((used))void *r;

static __attribute__ ((used))void* ptr () { return 0; }

static __attribute__ ((used))
void test (void *p, int n)
{
    {
        void *q = p;
        struct { void *p; } s = { p };

        // Verify that none of function arguments, local or global
        // variables, or function return values trigger the warning.
        new (p) char;
        new (q) char;
        new (r) char;
        new (s.p) char;
        new (ptr ()) char;

        new (p) char [32];
        new (q) char [32];
        new (r) char [32];
        new (s.p) char [32];
        new (ptr ()) char [32];

        new (&p) char;
        new (&q) char;
        new (&r) char;

        // Using address of objects, however, must trigger the warning.
        new (&p) char [32];                 // { dg-warning "placement" }
        new (&q) char [32];                 // { dg-warning "placement" }
        new (&r) char [32];                 // { dg-warning "placement" }
    }

    enum { N0, N1, N2, N3 };

    new (&c) char;

    // Warn for the common case when constructing at an offset from
    // the beginning of an array that doesn't leave enough space for
    // the object.
    new (&c + 0) char;                  // okay
    new (&c + n) char;                  // okay (n is unknown)
    new (&c + 1) char;                  // { dg-warning "placement" }
    new (&c + N0) char;
    new (&c + N1) char;                 // { dg-warning "placement" }

    // No warning is issued when constructing an array in space exactly
    // its size even though strictly speaking a compiler is allowed to
    // add a cookie to the array (gcc does not).
    new (&c) char [1];
    new (&c) char [sizeof c];
    new (&c) char [n];
    new (&c) char [1][1];
    new (&c) char [1][1][1];
    new (&c + N1) char [1][1][1];       // { dg-warning "placement" }

    new (&c) char [2];                  // { dg-warning "placement" }
    new (&c) char [sizeof c + 1];       // { dg-warning "placement" }
    new (&c) char [1][2];               // { dg-warning "placement" }
    new (&c) char [2][1];               // { dg-warning "placement" }
    new (&c) char [n][1];
    new (&c) char [n][2];               // { dg-warning "placement" }
    new (&c) char [3];                  // { dg-warning "placement" }
    new (&c) char [3][1];               // { dg-warning "placement" }
    new (&c) char [1][3];               // { dg-warning "placement" }
    new (&c) char [4][1];               // { dg-warning "placement" }
    new (&c) char [1][4];               // { dg-warning "placement" }

    // Casts must not suppress it.
    new ((void*)&c) char [2];           // { dg-warning "placement" }
    new ((char*)&c) char [3];           // { dg-warning "placement" }

    new (static_cast<void*>(&c)) char [4];        // { dg-warning "placement" }
    new (reinterpret_cast<char*>(&c)) char [5];   // { dg-warning "placement" }

    new (&c + 0) char [2];              // { dg-warning "placement" }
    new (&c + 0) char [3];              // { dg-warning "placement" }
    new (&c + 0) char [4];              // { dg-warning "placement" }

    new (&c + 1) char [2];              // { dg-warning "placement" }
    new (&c + 1) char [3];              // { dg-warning "placement" }
    new (&c + 1) char [4];              // { dg-warning "placement" }

    new (&c + N0) char [1];
    new (&c + N1) char [2];             // { dg-warning "placement" }

    // Warn even though n is unknown since c is too small for char[2]
    // regardless of the value of n.
    new (&c + n) char [2];              // { dg-warning "placement" }

    new (ac2) char [1];
    new (ac2) char [1][1];
    new (ac2) char [1][2];
    new (ac2) char [2][1];
    new (ac2) char [1][3];              // { dg-warning "placement" }
    new (ac2) char [2][2];              // { dg-warning "placement" }
    new (ac2) char [3][1];              // { dg-warning "placement" }

    new (ac2 + N0) char [1][1];
    new (ac2 + N0) char [1][2];
    new (ac2 + N1) char [1][2];         // { dg-warning "placement" }
    new (ac2 + N1) char [2][1];         // { dg-warning "placement" }
    new (ac2 + N2) char [1][1];         // { dg-warning "placement" }
    new (ac2 + N2) char [1][2];         // { dg-warning "placement" }
    new (ac2 + N2) char [2][1];         // { dg-warning "placement" }
    new (ac2 + N2) char [2][2];         // { dg-warning "placement" }

    new (ac8) char [1];
    new (ac8) char [2][2];
    new (ac8) char [2][3];
    new (ac8) char [2][4];
    new (ac8) char [2][5];              // { dg-warning "placement" }
    new (ac8) char [2][2][2];
    new (ac8) char [2][2][3];           // { dg-warning "placement" }

    new (&c) int;                       // { dg-warning "placement" }

    new (&ac1) int;                     // { dg-warning "placement" }
    new (&ac2) int;                     // { dg-warning "placement" }
    new (&ac3) int;                     // { dg-warning "placement" }
    new (&ac4) int;

    // Constructing at an address of an array element.
    new (&ac1 [0]) int;                 // { dg-warning "placement" }
    new (&ac2 [0]) int;                 // { dg-warning "placement" }
    new (&ac3 [0]) int;                 // { dg-warning "placement" }
    new (&ac4 [0]) int;

    // ...plus or minus a constant offset.
    new (&ac1 [0] + 0) int;             // { dg-warning "placement" }
    new (&ac2 [0] + 0) int;             // { dg-warning "placement" }
    new (&ac3 [0] + 0) int;             // { dg-warning "placement" }
    new (&ac4 [0] + 0) int;
    new (&ac4 [1] + 0) int;             // { dg-warning "placement" }
    new (&ac4 [1] - 1) int;
    new (&ac4 [2] - 1) int;             // { dg-warning "placement" }
    new (&ac4 [2] - 2) int;
    new (&ac4 [3] - 1) int;             // { dg-warning "placement" }
    new (&ac4 [3] - 2) int;             // { dg-warning "placement" }
    new (&ac4 [3] - 3) int;
    new (&ac4 [4] - 1) int;             // { dg-warning "placement" }
    new (&ac4 [4] - 2) int;             // { dg-warning "placement" }
    new (&ac4 [4] - 3) int;             // { dg-warning "placement" }
    new (&ac4 [4] - 4) int;

    new (&ac1 [0] + 1) int;             // { dg-warning "placement" }
    new (&ac2 [0] + 1) int;             // { dg-warning "placement" }
    new (&ac3 [0] + 1) int;             // { dg-warning "placement" }
    new (&ac4 [0] + 1) int;             // { dg-warning "placement" }

    new (&ac3 [0] + n) int;             // { dg-warning "placement" }
    new (&ac4 [0] + n) int;             // no warning (n could be zero)
    new (&ac4 [1] + n) int;             // no warning (n could be negative)
    new (&ac4 [2] + n) int;             // ditto
    new (&ac4 [3] + n) int;             // ditto
    new (&ac4 [4] + n) int;             // ditto
    new (&ac4 [4] - n) int;             // (or positive)

    new (&c + 0) int;                   // { dg-warning "placement" }
    new (&c + 1) int;                   // { dg-warning "placement" }

    // Constructing at an offset into the address of an array.
    new (&ac1 + 0) int;                 // { dg-warning "placement" }
    new (&ac1 + 1) int;                 // { dg-warning "placement" }
    new (&ac1 + n) int;                 // { dg-warning "placement" }
    new (&ac2 + 0) int;                 // { dg-warning "placement" }
    new (&ac2 + 1) int;                 // { dg-warning "placement" }
    new (&ac2 + n) int;                 // { dg-warning "placement" }
    new (&ac3 + 0) int;                 // { dg-warning "placement" }
    new (&ac3 + 1) int;                 // { dg-warning "placement" }

    // Even though n below is uknown an array of 3 bytes isn't large
    // enugh for an int.
    new (&ac3 + n) int;                 // { dg-warning "placement" }

    new (&ac4 + 0) int;
    new (&ac4 + 1) int;                 // { dg-warning "placement" }
    new (&ac4 + n) int;                 // no warning (n could be zero)

    // Constructing in an array object.
    new (ac1) int;                      // { dg-warning "placement" }
    new (ac2) int;                      // { dg-warning "placement" }
    new (ac3) int;                      // { dg-warning "placement" }
    new (ac4) int;
    new (ac5) int;
    new (ac5 + 0) int;
    new (ac5 + 1) int;
    new (ac5 + n) int;                  // no warning (n could be zero)
    new (ac5 + 2) int;                  // { dg-warning "placement" }
    new (ac5 + 3) int;                  // { dg-warning "placement" }
    new (ac5 + 4) int;                  // { dg-warning "placement" }
    new (ac5 + 5) int;                  // { dg-warning "placement" }

    new (ac1_1) char;
    new (ac1_1) char[1];
    new (ac1_1) char[n];                // no warning (n is unknown)
    new (ac1_1) char[2];                // { dg-warning "placement" }
    new (ac1_1) char[3];                // { dg-warning "placement" }

    new (ac1_2) char;
    new (ac1_2) char[1];
    new (ac1_2) char[2];
    new (ac1_2) char[3];                // { dg-warning "placement" }

    new (ac2_1) char;
    new (ac2_1) char[1];
    new (ac2_1) char[2];
    new (ac2_1) char[3];                // { dg-warning "placement" }

    new (ac2_2) char;
    new (ac2_2) char[1];
    new (ac2_2) char[2];
    new (ac2_2) char[2][2];

    // Even though n below is uknown it can't meaningfully be zero
    // (even if zero-size arrays are allowed as an extension, the size
    // they are allocated in by placement new is zero).
    new (ac1_1) char[n][2];             // { dg-warning "placement" }
    new (ac2_2) char[3];
    new (ac2_2) char[3][1];
    new (ac2_2) char[3][2];             // { dg-warning "placement" }
    new (ac2_2) char[4];
    new (ac2_2) char[4][1];
    new (ac2_2) char[4][2];             // { dg-warning "placement" }
    new (ac2_2) char[5];                // { dg-warning "placement" }

    new (&s) int;                       // { dg-warning "placement" }
    new (&as1) int;                     // { dg-warning "placement" }
    new (&as2) int;

    new (as1) int;                      // { dg-warning "placement" }
    new (as2) int;

    new (&sc.c) int;                    // { dg-warning "placement" }
    new (&sac1.ac) int;                 // { dg-warning "placement" }
    new (&sac2.ac) int;                 // { dg-warning "placement" }
    new (&sac3.ac) int;                 // { dg-warning "placement" }
    new (&sac4.ac) int;

    new (sc.pc) char;
    new (sc.pc) int;
    new (sc.pc) int[1024];
    new (sc.pc + 0) int;
    new (sc.pc + 0) int[2048];
    new (sc.pv) int;
    new (sc.pv) char[1024];

    new (sac1.ac) int;                  // { dg-warning "placement" }
    new (sac2.ac) int;                  // { dg-warning "placement" }
    new (sac3.ac) int;                  // { dg-warning "placement" }
    new (sac4.ac) int;

    new (&ssc.sc) SSC;                  // { dg-warning "placement" }
    new (&ssac1.sac) int;               // { dg-warning "placement" }
    new (&ssac2.sac) int;               // { dg-warning "placement" }
    new (&ssac3.sac) int;               // { dg-warning "placement" }
    new (&ssac4.sac) int;

    new (&sssac4_2) char[sizeof sssac4_2];
    new (&sssac4_2) char[sizeof sssac4_2 + 1];   // { dg-warning "placement" }

    // taking the address of a temporary is allowed with -fpermissive
    new (&fsc ().c) int;                // { dg-warning "address|placement" }
    new (&fasc1 ().ac) int;             // { dg-warning "address|placement" }
    new (&fasc2 ().ac) int;             // { dg-warning "address|placement" }
    new (&fasc3 ().ac) int;             // { dg-warning "address|placement" }
    new (&fasc4 ().ac) int;             // { dg-warning "address|placement" }

    new (&uac1) int;                    // { dg-warning "placement" }
    new (&uac2) int;                    // { dg-warning "placement" }
    new (&uac3) int;                    // { dg-warning "placement" }
    new (&uac4) int;
    new (&uac4 + 1) int;                // { dg-warning "placement" }

    new (&uac1.c) int;                  // { dg-warning "placement" }
    new (&uac2.c) int;                  // { dg-warning "placement" }
    new (&uac3.c) int;                  // { dg-warning "placement" }

    // Diagnose the following even though the size of uac4.c could be
    // expected to extend to the end of the union (as it is by Built-in
    // Object Size and so isn't diagnosed in calls to functions like
    // memset(&uac4.c, 0, sizeof(int)) when _FORTIFY_SOURCE is non-zero.  */
    new (&uac4.c) int;                  // { dg-warning "placement" }

    new (&uac4.c + 1) int;              // { dg-warning "placement" }
}


struct S { char c [2]; };

// Verify the full text of the warning message.
static  __attribute__ ((used))
void test_message (int i)
{
    char a [2];

    // The exact sizes of both the buffer and the type are known.
    new (a + 1) S;         // { dg-warning "placement new constructing an object of type .S. and size .2. in a region of type .char \\\[2\\\]. and size .1." }

    // The buffer size is known but only the size of the type whose
    // objects are being constructed is known, not their number.  While
    // in theory it could be zero, it practice likely never will be so
    // the potential false positive is acceptable.
    new (a + 1) S [i];  // { dg-warning "placement new constructing an array of objects of type .S. and size .2. in a region of type .char \\\[2\\\]. and size .1." }

    // The exact amount of space in the buffer isn't known, only its
    // maximum is.  The exact size of the array being created is known.
    new (a + i) S [2];  // { dg-warning "placement new constructing an object of type .S \\\[2\\\]. and size .4. in a region of type .char \\\[2\\\]. and size at most .2." }
}


struct ClassWithMemberNew {
    struct Object { int i; } *pobj;
    unsigned nobj;

    ClassWithMemberNew ();
    void foo ();

    void* operator new (size_t, void*);
    void* operator new[] (size_t, void*);
};

void ClassWithMemberNew::foo()
{
    for (unsigned i = 0; i != nobj; ++i)
        new (pobj + i) Object ();
}


struct ClassWithGlobalNew {
    int a [4];
    ClassWithGlobalNew ();
};

void* operator new (size_t, ClassWithGlobalNew*);
void* operator new[] (size_t, ClassWithGlobalNew*);

void test_user_defined_placement_new ()
{
    {
        ClassWithMemberNew x;

        // Expect no diagnostics for placement new expressions with types
        // with their own placement operator new since the semantics of
        // the operator aren't known.
        new (&c) ClassWithMemberNew;
        new (&x) ClassWithMemberNew[2];
    }

    {
        ClassWithGlobalNew x;

        new (&c) ClassWithGlobalNew;    // { dg-warning "placement" }
        new (&x) ClassWithGlobalNew[2];
    }
}

extern char extbuf[];

template <class> struct TemplateClass { char c; };

// Declare a specialization but don't provide a definition.
template <> struct TemplateClass<void>;

// Declare an object of an explicit specialization of an unknown size.
extern TemplateClass<void> exttempl_void;

// Verify that no warning is issued when placement new is called with
// an extern buffer of unknown size (and the case is handled gracefully
// and doesn't cause an ICE).
static __attribute__ ((used))
void test_extern_buffer_of_unknown_size ()
{
    new (extbuf) int ();
    new (extbuf) int [1024];

    new (&exttempl_void) int ();
    new (&exttempl_void) int [1024];
}

extern char extbuf_size_int [sizeof (int)];

extern TemplateClass<int> exttempl;

// Verify that a warning is issued as expected when placement new is
// called with an extern buffer of known size (and the case is handled
// gracefully and doesn't cause an ICE).
static __attribute__ ((used))
void test_extern_buffer ()
{
    new (extbuf_size_int) int ();
    new (extbuf_size_int) int [1];

    struct S { int a [2]; };

    new (extbuf_size_int) S;            // { dg-warning "placement" }
    new (extbuf_size_int) int [2];      // { dg-warning "placement" }

    new (&exttempl) int ();             // { dg-warning "placement" }
    new (&exttempl) int [1024];         // { dg-warning "placement" }
}
