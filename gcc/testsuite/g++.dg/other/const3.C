// PR middle-end/44101
// { dg-do compile }

extern bool equal (int[], int[], const int[]);
extern bool equal (wchar_t[], wchar_t[], const wchar_t[]);

void foo(void)
{
  const int A1[] = {3, 3, 3, 3, 3, 3, 3, 3, 3, 3};
  const int N1 = sizeof(A1) / sizeof(int);
  int i1[N1];

  if (equal(i1, i1 + N1, A1))
    return;

  const wchar_t A3[] = {L'\3', L'\3', L'\3', L'\3', L'\3',
   L'\3', L'\3', L'\3', L'\3', L'\3'};
  const int N3 = sizeof(A3) / sizeof(wchar_t);
  wchar_t i3[N3];

  if (equal(i3, i3 + N3, A3))
    return;
}
