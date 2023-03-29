// https://issues.dlang.org/show_bug.cgi?id=23532
// DISABLED: win32 win64
struct _Complex(T)
{
    T re;
    T im;
    static @property epsilon()()    { return _Complex(T.epsilon, T.epsilon); }
    static @property infinity()()   { return _Complex(T.infinity, T.infinity); }
    static @property max()()        { return _Complex(T.max, T.max); }
    static @property min_normal()() { return _Complex(T.min_normal, T.min_normal); }
    static @property nan()()        { return _Complex(T.nan, T.nan); }
    static @property dig()()        { return T.dig; }
    static @property mant_dig()()   { return T.mant_dig; }
    static @property max_10_exp()() { return T.max_10_exp; }
    static @property max_exp()()    { return T.max_exp; }
    static @property min_10_exp()() { return T.min_10_exp; }
    static @property min_exp()()    { return T.min_exp; }
}

enum __c_complex_float  : _Complex!float;
enum __c_complex_double : _Complex!double;
enum __c_complex_real   : _Complex!real;

static assert(__c_complex_float.epsilon is _Complex!float.epsilon);
static assert(__c_complex_float.infinity is _Complex!float.infinity);
static assert(__c_complex_float.init is _Complex!float.init);
static assert(__c_complex_float.max is _Complex!float.max);
static assert(__c_complex_float.min_normal is _Complex!float.min_normal);
static assert(__c_complex_float.nan is _Complex!float.nan);
static assert(__c_complex_float.sizeof == _Complex!float.sizeof);
static assert(__c_complex_float.alignof == _Complex!float.alignof);
static assert(__c_complex_float.dig == _Complex!float.dig);
static assert(__c_complex_float.mant_dig == _Complex!float.mant_dig);
static assert(__c_complex_float.max_10_exp == _Complex!float.max_10_exp);
static assert(__c_complex_float.max_exp == _Complex!float.max_exp);
static assert(__c_complex_float.min_10_exp == _Complex!float.min_10_exp);
static assert(__c_complex_float.min_exp == _Complex!float.min_exp);

static assert(__c_complex_double.epsilon is _Complex!double.epsilon);
static assert(__c_complex_double.infinity is _Complex!double.infinity);
static assert(__c_complex_double.init is _Complex!double.init);
static assert(__c_complex_double.max is _Complex!double.max);
static assert(__c_complex_double.min_normal is _Complex!double.min_normal);
static assert(__c_complex_double.nan is _Complex!double.nan);
static assert(__c_complex_double.sizeof == _Complex!double.sizeof);
static assert(__c_complex_double.alignof == _Complex!double.alignof);
static assert(__c_complex_double.dig == _Complex!double.dig);
static assert(__c_complex_double.mant_dig == _Complex!double.mant_dig);
static assert(__c_complex_double.max_10_exp == _Complex!double.max_10_exp);
static assert(__c_complex_double.max_exp == _Complex!double.max_exp);
static assert(__c_complex_double.min_10_exp == _Complex!double.min_10_exp);
static assert(__c_complex_double.min_exp == _Complex!double.min_exp);

static assert(__c_complex_real.epsilon is _Complex!real.epsilon);
static assert(__c_complex_real.infinity is _Complex!real.infinity);
static assert(__c_complex_real.init is _Complex!real.init);
static assert(__c_complex_real.max is _Complex!real.max);
static assert(__c_complex_real.min_normal is _Complex!real.min_normal);
static assert(__c_complex_real.nan is _Complex!real.nan);
static assert(__c_complex_real.sizeof == _Complex!real.sizeof);
static assert(__c_complex_real.alignof == _Complex!real.alignof);
static assert(__c_complex_real.dig == _Complex!real.dig);
static assert(__c_complex_real.mant_dig == _Complex!real.mant_dig);
static assert(__c_complex_real.max_10_exp == _Complex!real.max_10_exp);
static assert(__c_complex_real.max_exp == _Complex!real.max_exp);
static assert(__c_complex_real.min_10_exp == _Complex!real.min_10_exp);
static assert(__c_complex_real.min_exp == _Complex!real.min_exp);
