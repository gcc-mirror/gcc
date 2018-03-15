// Used by main.C

module;

extern "C" int printf (const char *, ...);
export module helgen;

export void greeter (const char *name)
{
  printf ("Hello %s!\n", name);
}
