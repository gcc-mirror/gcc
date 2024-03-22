extern const char XtStrings[];

void unknown_fn (void *);

void test (void)
{
  unknown_fn ((char*)&XtStrings[429]);
}
