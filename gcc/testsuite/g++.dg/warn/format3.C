// PR c++/13070
// { dg-do compile }
// { dg-options "-Wformat" }

extern "C" int printf (const char*, ...);

int main()
{
  printf("%d\n", 1, 1);  // { dg-warning "too many" "printf warning" }
  return 0;
}

