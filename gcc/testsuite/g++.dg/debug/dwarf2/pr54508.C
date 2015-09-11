// PR debug/54508
// { dg-do compile }
// { dg-options "-gdwarf-2 -g2 -dA -fno-merge-debug-strings" }

// { dg-final { scan-assembler "\"cbase\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"OPCODE\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"bi\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"si\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"f1\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"f2\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler-not "\"nc\\\\0\"\[ \t\]+\# +DW_AT_name" } }

class cbase

{
public:
 static int si;
    int bi;
};

class c : public cbase

{
public:
 enum
 {
  OPCODE = 251
 };
 int i ;
 static const char *testc (void) { return "foo"; }
};

struct s
{
    int f1;
    static const char *tests (void) { return "test"; }
};

union u
{
    int f2;
    double d;
    static const char *testu (void) { return "test union"; }
};

namespace n 
{
    const char *ntest (void) { return "test n"; }

    class nc
    {
    public:
        int i;
        static int sj;
    };
}

extern void send (int, int, const void *, int);

void test (int src)
{
  int cookie = 1;
  send(src, c::OPCODE, c::testc (), cookie);
  send(src, c::OPCODE, s::tests (), cookie);
  send(src, c::OPCODE, u::testu (), cookie);
  send(src, c::OPCODE, n::ntest (), cookie);
}
