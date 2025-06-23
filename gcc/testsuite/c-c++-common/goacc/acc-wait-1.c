// { dg-do compile }
// { dg-additional-options "-fdump-tree-original" }

void f0 (int x)
{
  #pragma acc wait(x) if(false) async
}
// { dg-final { scan-tree-dump-times "if \\(0\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-1, 1, x\\);" 1 "original" { target c } } }
// { dg-final { scan-tree-dump-not "__builtin_GOACC_wait \\(-1, 1, x\\);" "original" { target c++ } } }


void f1 (int y, int ia)
{
  #pragma acc wait(y) if(true) async(ia)
}
// { dg-final { scan-tree-dump-times "if \\(1\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(ia, 1, y\\);" 1 "original" { target c } } }
// { dg-final { scan-tree-dump-times "__builtin_GOACC_wait \\(ia, 1, y\\)" 1 "original" { target c++ } } }
// { dg-final { scan-tree-dump-not "if \\(\[^\\n\\r\]+\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(ia, 1, y\\);" "original" { target c++ } } }


void fl (int z, bool ll)
{
  #pragma acc wait(z) if(ll) async(3)
}
// { dg-final { scan-tree-dump-times "if \\(ll != 0\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(3, 1, z\\);" 1 "original" { target c } } }
// { dg-final { scan-tree-dump-times "if \\(ll\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(3, 1, z\\);" 1 "original" { target c++ } } }


void a0 (int a)
{
  #pragma acc wait(a) if(false)
}
// { dg-final { scan-tree-dump-times "if \\(0\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, a\\);" 1 "original" { target c } } }
// { dg-final { scan-tree-dump-not "__builtin_GOACC_wait \\(-2, 1, a\\);" "original" { target c++ } } }


void a1 (int b)
{
  #pragma acc wait(b) if(true)
}
// { dg-final { scan-tree-dump-times "if \\(1\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, b\\);" 1 "original" { target c } } }
// { dg-final { scan-tree-dump-times "__builtin_GOACC_wait \\(-2, 1, b\\)" 1 "original" { target c++ } } }
// { dg-final { scan-tree-dump-not "if \\(\[^\\n\\r\]+\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, b\\);" "original" { target c++ } } }


void al (int c, bool qq)
{
  #pragma acc wait(c) if(qq)
}
// { dg-final { scan-tree-dump-times "if \\(qq != 0\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, c\\);" 1 "original" { target c } } }
// { dg-final { scan-tree-dump-times "if \\(qq\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, c\\);" 1 "original" { target c++ } } }
