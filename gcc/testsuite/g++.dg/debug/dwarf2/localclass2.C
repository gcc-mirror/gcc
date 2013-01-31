// PR c++/27017
// { dg-do compile }
// { dg-options "-gdwarf-2 -dA -O2 -feliminate-unused-debug-types -fno-merge-debug-strings" }

int
foo (int arg1)
{
  struct localstruct1
  {
    static inline int staticfn1 (int arg2)
    {
      int var2 = arg2 << 2;
      return arg2 + var2;
    }
    static int staticfn2 (int arg3)
    {
      int var3 = arg3 << 2;
      return arg3 + var3;
    }
    static inline int staticfn3 (int arg4)
    {
      int var4 = arg4 << 2;
      return arg4 + var4;
    }
    static int staticfn4 (int arg5)
    {
      int var5 = arg5 << 2;
      return arg5 + var5;
    }
    int method1 (int arg6)
    {
      int var6 = arg6 << 2;
      return arg6 + var6;
    }
  };
  struct localstruct2
  {
    static inline int staticfn5 (int arg7)
    {
      int var7 = arg7 << 2;
      return arg7 + var7;
    }
    static int staticfn6 (int arg8)
    {
      int var8 = arg8 << 2;
      return arg8 + var8;
    }
  };
  return localstruct1::staticfn1 (arg1) + localstruct1::staticfn2 (arg1);
}

int
main ()
{
  return foo (1) - 10;
}

// { dg-final { scan-assembler "main\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "foo\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "staticfn1\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "staticfn2\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "staticfn3\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "staticfn4\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "staticfn5\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "staticfn6\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "method1\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "arg1\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "arg2\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "arg3\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "arg4\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "arg5\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "arg6\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "arg7\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "arg8\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler "localstruct1\[^\n\r\]*DW_AT_name" } }
// { dg-final { scan-assembler-not "localstruct2\[^\n\r\]*DW_AT_name" } }
