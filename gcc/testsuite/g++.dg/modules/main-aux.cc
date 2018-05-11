// Used by main_a.C

module;

extern "C" int printf (const char *, ...);
export module main.aux;

export void greeter (const char *name)
{
  printf ("Hello %s!\n", name);
}
