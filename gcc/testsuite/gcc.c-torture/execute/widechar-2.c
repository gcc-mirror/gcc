#include <stddef.h>

void abort(void);
void exit(int);

const wchar_t ws[] = L"foo";

int
main (void)
{
  if (ws[0] != L'f' || ws[1] != L'o' || ws[2] != L'o' || ws[3] != L'\0')
    abort();
  exit(0);
}
