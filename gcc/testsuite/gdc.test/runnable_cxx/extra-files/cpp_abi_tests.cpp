struct S{
    float a;
};

bool               passthrough(bool                value)     { return value; }
signed char        passthrough(signed char         value)     { return value; }
unsigned char      passthrough(unsigned char       value)     { return value; }
char               passthrough(char                value)     { return value; }
wchar_t            passthrough(wchar_t             value)     { return value; }
short              passthrough(short               value)     { return value; }
unsigned short     passthrough(unsigned short      value)     { return value; }
int                passthrough(int                 value)     { return value; }
unsigned int       passthrough(unsigned int        value)     { return value; }
long               passthrough(long                value)     { return value; }
unsigned long      passthrough(unsigned long       value)     { return value; }
long long          passthrough(long long           value)     { return value; }
unsigned long long passthrough(unsigned long long  value)     { return value; }
float              passthrough(float               value)     { return value; }
double             passthrough(double              value)     { return value; }
S                  passthrough(S                   value)     { return value; }

bool               passthrough_ptr(bool               *value) { return *value; }
signed char        passthrough_ptr(signed char        *value) { return *value; }
unsigned char      passthrough_ptr(unsigned char      *value) { return *value; }
char               passthrough_ptr(char               *value) { return *value; }
wchar_t            passthrough_ptr(wchar_t            *value) { return *value; }
short              passthrough_ptr(short              *value) { return *value; }
unsigned short     passthrough_ptr(unsigned short     *value) { return *value; }
int                passthrough_ptr(int                *value) { return *value; }
unsigned int       passthrough_ptr(unsigned int       *value) { return *value; }
long               passthrough_ptr(long               *value) { return *value; }
unsigned long      passthrough_ptr(unsigned long      *value) { return *value; }
long long          passthrough_ptr(long long          *value) { return *value; }
unsigned long long passthrough_ptr(unsigned long long *value) { return *value; }
float              passthrough_ptr(float              *value) { return *value; }
double             passthrough_ptr(double             *value) { return *value; }
S                  passthrough_ptr(S                  *value) { return *value; }

bool               passthrough_ref(bool               &value) { return value; }
signed char        passthrough_ref(signed char        &value) { return value; }
unsigned char      passthrough_ref(unsigned char      &value) { return value; }
char               passthrough_ref(char               &value) { return value; }
wchar_t            passthrough_ref(wchar_t            &value) { return value; }
short              passthrough_ref(short              &value) { return value; }
unsigned short     passthrough_ref(unsigned short     &value) { return value; }
int                passthrough_ref(int                &value) { return value; }
unsigned int       passthrough_ref(unsigned int       &value) { return value; }
long               passthrough_ref(long               &value) { return value; }
unsigned long      passthrough_ref(unsigned long      &value) { return value; }
long long          passthrough_ref(long long          &value) { return value; }
unsigned long long passthrough_ref(unsigned long long &value) { return value; }
float              passthrough_ref(float              &value) { return value; }
double             passthrough_ref(double             &value) { return value; }
S                  passthrough_ref(S                  &value) { return value; }

// Uncomment when mangling is fixed
// typedef void(*fn0)();
// fn0            passthrough_fn0   (fn0 value) { return value; }
// typedef int (*fn1)(int);
// fn1            passthrough_fn1   (fn1 value) { return value; }
