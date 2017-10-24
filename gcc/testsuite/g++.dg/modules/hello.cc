// Used by main.C

extern "C" int printf (const char *, ...);
export module hello;

export void greeter (const char *name)
{
  printf ("Hello %s!\n", name);
}
