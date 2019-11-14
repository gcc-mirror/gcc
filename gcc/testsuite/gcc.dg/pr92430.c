// PR rtl-optimization/92430
// { dg-do compile }
// { dg-options "-Os -fno-if-conversion -fno-tree-dce -fno-tree-loop-optimize -fno-tree-vrp" }

int eb, ko;

void
e9 (int pe, int lx)
{
  int ir;

  for (ir = 0; ir < 1; ++ir)
    {
      for (ko = 0; ko < 1; ++ko)
        {
          for (eb = 0; eb < 1; ++eb)
            ko += pe;

          for (ko = 0; ko < 1; ++ko)
            ;
        }

      pe = ir = lx;
    }
}
