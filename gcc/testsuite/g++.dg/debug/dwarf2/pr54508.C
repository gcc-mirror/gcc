// PR debug/54508
// { dg-do compile }
// { dg-options "-g2 -dA -fno-merge-debug-strings" }

// { dg-final { scan-assembler-not "\"cbase\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"c\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name\[\r\n\]+\[\^\r\n\]+\[\r\n\]+\[\^\r\n\]+\[\r\n\]+\[\^#;/!|@\]+\[#;/!|@\]+ +DW_AT_decl_line\[\r\n\]+\[\^#;/!|@\]+\[#;/!|@\]+ +DW_AT_declaration" } }
// { dg-final { scan-assembler-not "\"OPCODE\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler-not "\"bi\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler-not "\"si\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"s\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler-not "\"s\\\\0\"\[^#;/!|@\]+\[#;/!|@\]+ +DW_AT_name\[\r\n\]+\[\^\r\n\]+\[\r\n\]+\[\^\r\n\]+\[\r\n\]+\[\^#;/!|@\]+\[#;/!|@\]+ +DW_AT_decl_line\[\r\n\]+\[ \t\]+\[#;/!|@\]+ +DW_AT_declaration" } }
// { dg-final { scan-assembler "\"f1\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
// { dg-final { scan-assembler "\"u\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name\[\r\n\]+\[\^\r\n\]+\[\r\n\]+\[\^\r\n\]+\[\r\n\]+\[\^#;/!|@\]+\[#;/!|@\]+ +DW_AT_decl_line\[\r\n\]+\[^#;/!|@\]+\[#;/!|@\]+ +DW_AT_declaration" } }
// { dg-final { scan-assembler-not "\"f2\\\\0\"\[ \t\]+\[#;/!|@\]+ +DW_AT_name" } }
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
  static struct s ss;
  
  send(src, c::OPCODE, c::testc (), cookie);
  send(src, c::OPCODE, u::testu (), cookie);
  send(src, c::OPCODE, n::ntest (), cookie);
}
