// PR gcov-profile/97069
// { dg-options "--coverage" }
// { dg-do run { target native } }

# 0 "pr97069.C"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "pr97069.C"
int main()
{
  return 0;
}
# 0 "pr97069.C"
void zero_line_directive()
{
}

// { dg-final { run-gcov pr97069.C } }
