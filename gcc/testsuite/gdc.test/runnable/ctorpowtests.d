// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:

int magicVariable()
{
  if (__ctfe)
   return 3;

  shared int var = 2;
  return var;
}

static assert(magicVariable()==3);

void main()
{
  assert(!__ctfe);
  assert(magicVariable()==2);
}

// bug 991 -- invalid.
// bug 3500 -- is this related to 2127?

// Tests for ^^
// TODO: These tests should not require import std.math.

import std.math;
// Test float ^^ int
static assert( 27.0 ^^ 5 == 27.0 * 27.0 * 27.0 * 27.0 * 27.0);
static assert( 2.0 ^^ 3 == 8.0);

static assert( 2.0 ^^ 4 == 16.0);
static assert( 2 ^^ 4 == 16);

// Check the typing rules.
static assert( is (typeof(2.0^^7) == double));
static assert( is (typeof(7^^3) == int));

static assert( is (typeof(7L^^3) == long));
static assert( is (typeof(7^^3L) == long));
enum short POW_SHORT_1 = 3;
enum short POW_SHORT_3 = 7;
static assert( is (typeof(POW_SHORT_1 * POW_SHORT_1) ==
typeof(POW_SHORT_1*POW_SHORT_1)));

static assert( is (typeof(7.0^^3) == double));
static assert( is (typeof(7.0L^^3) == real));
static assert( is (typeof(7.0f^^3) == float));
static assert( is (typeof(POW_SHORT_1^^3.1) == double));
static assert( is (typeof(POW_SHORT_1^^3.1f) == float));
static assert( is (typeof(2.1f ^^ POW_SHORT_1) == float));
static assert( is (typeof(7.0f^^3.1) == double));
static assert( is (typeof(7.0^^3.1f) == double));
static assert( is (typeof(7.0f^^3.1f) == float));
static assert( is (typeof(7.0f^^3.1L) == real));
static assert( is (typeof(7.0L^^3.1f) == real));
// Check typing for special cases
static assert( is (typeof(7.0f^^2) == float));
static assert( is (typeof(7.0f^^2.0) == double));
static assert( is (typeof(7.0f^^8.0) == double));
static assert( is (typeof(1^^0.5f) == float));
static assert( is (typeof(7^^0.5f) == float));
static assert( is (typeof(3L^^0.5) == double));
static assert( is (typeof(123^^17.0f) == float));

static assert(POW_SHORT_1 ^^ 2 == 9);
static assert(4.0 ^^ POW_SHORT_1 == 4.0*4.0*4.0);
static assert(4.0 ^^ 7.0 == 4.0*4.0*4.0*4.0*4.0*4.0*4.0);

// ^^ has higher precedence than multiply
static assert( 2 * 2 ^^ 3 + 1 == 17);
static assert( 2 ^^ 3 * 2 + 1 == 17);
// ^^ has higher precedence than negate
static assert( -2 ^^ 3 * 2 - 1 == -17);

// ^^ is right associative
static assert( 2 ^^ 3 ^^ 2 == 2 ^^ 9);
static assert( 2.0 ^^ -3 ^^ 2 == 2.0 ^^ -9);

// 1 ^^ n is always 1, even if n is negative
static assert( 1 ^^ -5 == 1);

// -1 ^^ n gets transformed into  n & 1 ? -1 : 1
// even if n is negative
static assert( (-1) ^^ -5 == -1);
static assert( (-1) ^^ -4 == 1);
static assert( (-1) ^^ 0 == 1);

// n ^^ 0 is always 1
static assert( (-5) ^^ 0 == 1);

// n ^^ 1 is always n
static assert( 6.0 ^^ 1 == 6.0);

// n ^^ -1.0 gets transformed into 1.0 / n, even if n is negative
static assert( (-4) ^^ -1.0 == 1.0 / -4);
static assert( 9 ^^ -1.0 == 1.0 / 9);

// Other integers raised to negative powers create an error
static assert( !is(typeof(2 ^^ -5)));
static assert( !is(typeof((-2) ^^ -4)));

// Bug 3535
struct StructWithCtor
{
    this(int _n) {
        n = _n; x = 5;
    }
    this(int _n, float _x) {
        n = _n; x = _x;
    }
    int n;
    float x;
}

int containsAsm()
{
    version (D_InlineAsm_X86)
        asm { nop; }
    else version (D_InlineAsm_X86_64)
        asm { nop; }
    return 0;
}

enum A = StructWithCtor(1);
enum B = StructWithCtor(7, 2.3);

static assert(A.n == 1);
static assert(A.x == 5.0);
static assert(B.n == 7);
static assert(B.x == 2.3);

int bazra(int x)
{
   StructWithCtor p = StructWithCtor(4);
   return p.n ^^ 3;

}

static assert(bazra(14)==64);

void moreCommaTests()
{
   auto k = (containsAsm(), containsAsm());
   for (int i=0; i< k^^2; i+=StructWithCtor(1).n) {}
}

// Test copy constructors
struct CopyTest {
   double x;
   this(double a) { x = a * 10.0;}
   this(this) {  x+=2.0;}
}

struct CopyTest2
{
   int x; int x1; int x2; int x3;
   this(int a) { x = a * 2; x1 = 3;}
   this(this) {  x1+=17;}
}


const CopyTest z = CopyTest(5.3);
/+
// TODO: This is not yet supported. But it
// generates an error message instead of wrong-code.
const CopyTest w = z;
static assert(z.x==55.0);
+/

int copytest1()
{
   CopyTest z = CopyTest(3.4);
   CopyTest w = z;
   assert(w.x == 36.0);
   CopyTest2 q = CopyTest2(7);
   CopyTest2 q2 = q;
   CopyTest2 q3 = q2;
   assert(q3.x1 == 37);

  return 123;
}
static assert(copytest1()==123);

// This must not cause a segfault
alias int FILTH;
struct Filth
{
     struct Impl
    {
        FILTH * handle = null;
        this(FILTH* h, uint r, string n)
        {
            handle = h;
        }
    }
    Impl * p;

    this(string name, in char[] stdioOpenmode = "rb")
    {
    }

    ~this()
    {
        if (!p) return;
    }

    this(this)
    {
        if (!p) return;
    }
    }
    struct InputByChar
    {
        private Filth _f;

        this(Filth f)
        {
            _f = f;
        }
}


static int nastyForCtfe=4;

// Can't use a global variable
static assert(!is(typeof( (){ static assert(0!=nastyForCtfe^^2); })));

int anotherPowTest()
{
   double x = 5.0;
   return x^^4 > 2.0 ? 3: 2;
}
