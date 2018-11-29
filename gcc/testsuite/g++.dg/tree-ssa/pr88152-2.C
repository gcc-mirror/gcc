// PR target/88152
// { dg-do compile { target int32 } }
// { dg-options "-O2 -Wno-psabi -fdump-tree-forwprop1" }
// { dg-final { scan-tree-dump-not " (?:>|>=|<|<=) \{ 214748364\[67]" "forwprop1" } }
// { dg-final { scan-tree-dump-not " (?:>|>=|<|<=) \{ -214748364\[78]" "forwprop1" } }
// { dg-final { scan-tree-dump-times "(?:return| =) \{ 0, 0, 0, 0 \}" 2 "forwprop1" } }
// { dg-final { scan-tree-dump-times "(?:return| =) \{ -1, -1, -1, -1 \}" 2 "forwprop1" } }
// { dg-final { scan-tree-dump-times " == \{ 2147483647, 2147483647, 2147483647, 2147483647 \}" 2 "forwprop1" } }
// { dg-final { scan-tree-dump-times " != \{ 2147483647, 2147483647, 2147483647, 2147483647 \}" 2 "forwprop1" } }
// { dg-final { scan-tree-dump-times " == \{ -2147483648, -2147483648, -2147483648, -2147483648 \}" 2 "forwprop1" } }
// { dg-final { scan-tree-dump-times " != \{ -2147483648, -2147483648, -2147483648, -2147483648 \}" 2 "forwprop1" } }

typedef int V __attribute__((vector_size (16)));

V
f1 (V a)
{
  return a > __INT_MAX__;
}

V
f2 (V a)
{
  return a >= __INT_MAX__;
}

V
f3 (V a)
{
  return a < __INT_MAX__;
}

V
f4 (V a)
{
  return a <= __INT_MAX__;
}

V
f5 (V a)
{
  return a > -__INT_MAX__ - 1;
}

V
f6 (V a)
{
  return a >= -__INT_MAX__ - 1;
}

V
f7 (V a)
{
  return a < -__INT_MAX__ - 1;
}

V
f8 (V a)
{
  return a <= -__INT_MAX__ - 1;
}

V
f9 (V a)
{
  return a > __INT_MAX__ - 1;
}

V
f10 (V a)
{
  return a <= __INT_MAX__ - 1;
}

V
f11 (V a)
{
  return a >= -__INT_MAX__;
}

V
f12 (V a)
{
  return a < -__INT_MAX__;
}
