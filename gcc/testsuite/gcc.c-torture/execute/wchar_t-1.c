typedef __WCHAR_TYPE__ wchar_t;
wchar_t x[] = L"Ä";
wchar_t y = L'Ä';
extern void abort (void);
extern void exit (int);

int main (void)
{
  if (sizeof (x) / sizeof (wchar_t) != 2)
    abort ();
  if (x[0] != L'Ä' || x[1] != L'\0')
    abort ();
  if (y != L'Ä')
    abort ();
  exit (0);
}
